--!
--!
--!
--!
--!
--!

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package uart_pkg is

    -- CONFIGURATION CONSTANTS
    constant CFG_CHARSIZE_SIZE  : natural := 2;
    constant CFG_CHARSIZE_8BIT  : std_logic_vector(1 downto 0) := "11";
    constant CFG_CHARSIZE_7BIT  : std_logic_vector(1 downto 0) := "10";
    constant CFG_CHARSIZE_6BIT  : std_logic_vector(1 downto 0) := "01";
    constant CFG_CHARSIZE_5BIT  : std_logic_vector(1 downto 0) := "00";

    constant CFG_PARITY_TYPE_EVEN : std_logic := '0';
    constant CFG_PARITY_TYPE_ODD  : std_logic := '1';

    constant CFG_STOP_BITS_1 : std_logic := '0';
    constant CFG_STOP_BITS_2 : std_logic := '1';

    constant CFG_AUTOBAUD_MODE_SIZE   : natural := 2;
    constant CFG_AUTOBAUD_MODE_B_01X  : std_logic_vector(1 downto 0) := "00";
    constant CFG_AUTOBAUD_MODE_B_010X : std_logic_vector(1 downto 0) := "01";
    constant CFG_AUTOBAUD_MODE_H_7F   : std_logic_vector(1 downto 0) := "10";
    constant CFG_AUTOBAUD_MODE_H_55   : std_logic_vector(1 downto 0) := "11";

end package;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.uart_pkg.all;

entity uart is
  generic (
    FCLK         : natural;                                                           --! Frequency of i_clk, the value specified must be at least 16 time the maximum supported baudrate
    OVERSAMPLE   : natural := 16;                                                     --! Oversampling factor of the UART
    RST_VALUE    : std_logic                                                          --! Reset value of i_rst
  );
  port (
    -- CONTROL I/F
    i_clk           : in  std_logic;                                                  --! Input clock for the interfaces
    i_rst           : in  std_logic;                                                  --! Reset, synchronous signal
    i_en            : in  std_logic;                                                  --! Enable the UART

    -- UART PHY I/F
    o_tx            : out std_logic;                                                  --! PHY interface for TX signal
    i_rx            : in  std_logic;                                                  --! PHY interface for RX signal

    -- UART DATA I/F
    i_tx_we         : in  std_logic;                                                  --! Write Enable
    i_tx_data       : in  std_logic_vector(7 downto 0);                               --! Transmit data
    o_tx_data_rdy   : out std_logic;                                                  --! Ready to read tx_data

    i_rx_re         : in  std_logic;                                                  --! Read enable for the RX data
    o_rx_data       : out std_logic_vector(7 downto 0);                               --! Received data
    o_rx_data_rdy   : out std_logic;                                                  --! RX Data Ready

    -- ERROR I/F
    o_err_parity    : out std_logic;                                                  --! Receiver detected that data doesnt match parity bit
    o_err_framing   : out std_logic;                                                  --! Receiver did not see the stop bit at expected stop time
    o_err_overrun   : out std_logic;                                                  --! Receiver buffer full and tried to write a new data (new data lost)
    o_err_underrun  : out std_logic;                                                  --! TX buffer & TX Shift reg empty and tried to read a value

    -- CONFIGURATION I/F
    i_cfg_charsize        : in  std_logic_vector(CFG_CHARSIZE_SIZE-1 downto 0);       --! Character size  5(00),6(01),7(10),8(11)
    i_cfg_parity_en       : in  std_logic;                                            --! Parity bit enable: 0 disable, 1 enable
    i_cfg_parity_type     : in  std_logic;                                            --! Party type: Even (0), Odd (1)
    i_cfg_stop_bits       : in  std_logic;                                            --! Number of stop bits 1('0'), 2('1')
    i_cfg_autobaud_en     : in  std_logic;                                            --! Autobaud enable : Not implemented
    i_cfg_autobaud_mode   : in  std_logic_vector(CFG_AUTOBAUD_MODE_SIZE-1 downto 0);  --! Autobaud mode -- 00: 01X mode, 01: 010X mode, 10: 0x7F mode, 11: 0x55 mode, 10 & 11 mode unsuported when charsize /= 11
    -- FCR Fifo Control Register : Not implemented
    -- BRHR & BRLR Baud Rate High and Low registers
    i_cfg_baudrate        : in  std_logic_vector(15 downto 0)                         --! i_cfg_baudrate = FCLK/(OVERSAMPLE*BAUDRATE) - 1
  );
end entity uart;

architecture rtl of uart is

  -- TODO: Fifo

  signal err_framing : std_logic;

begin

  ----------------------------------------------------------------------
  -- RX
  ----------------------------------------------------------------------

  b_rx:
  block
    type rx_state_t is (STANDBY, START, RX, STOP, PARITY, BREAK, OVERRUN);
    signal state, next_state : rx_state_t;

    type reg_t is record
      data   : std_logic_vector(7 downto 0);
      full   : std_logic;
      wr, rd : std_logic;
      fe, pe : std_logic; -- Framing Err & Parity Err
    end record;
    signal buf : reg_t;

    signal rx_sync_ff1, rx_sync_ff2 : std_logic;
    signal rx_bit, rx_bit_ff : std_logic;
    signal parity_bit : std_logic;
    signal bit_cntr   : unsigned(2 downto 0); -- 0 to 7 encoded

    signal oversample_cntr : unsigned(i_cfg_baudrate'range);
    signal oversample_clken : std_logic;

    signal baud_cntr : unsigned(3 downto 0);
    signal sample     : std_logic;

    signal shift_reg  : std_logic_vector(7 downto 0);
    signal rx_val     : std_logic_vector(7 downto 0);

  begin

    rx_sync_ff1 <= i_rx when rising_edge(i_clk);
    rx_sync_ff2 <= rx_sync_ff1 when rising_edge(i_clk);

    rx_bit    <= rx_sync_ff2;
    rx_bit_ff <= rx_sync_ff2 when rising_edge(i_clk);


    p_oversample_cntr:
    process(i_clk)
    begin
      if rising_edge(i_clk) then
        if state = STANDBY then
          if next_state = START and unsigned(i_cfg_baudrate) /= 0 then
            oversample_cntr <= oversample_cntr - 1;
          else
            oversample_cntr <= unsigned(i_cfg_baudrate);
          end if;
        elsif oversample_clken = '1' then
          oversample_cntr <= unsigned(i_cfg_baudrate);
        else
          oversample_cntr <= oversample_cntr - 1;
        end if;
      end if;
    end process;

    oversample_clken <= '1' when oversample_cntr = 0 else '0';


    p_baud_cntr:
    process(i_clk)
    begin
      if rising_edge(i_clk) then
        if i_rst = RST_VALUE or state = STANDBY then
          baud_cntr <= to_unsigned(OVERSAMPLE/2 - 1, baud_cntr'length);
        elsif oversample_clken = '1' then
          if baud_cntr = 0 then
            baud_cntr <= to_unsigned(OVERSAMPLE-1, baud_cntr'length);
          else
            baud_cntr <= baud_cntr - 1;
          end if;
        end if;
      end if;
    end process;

    sample <= '1' when baud_cntr = 0 and oversample_clken = '1' else '0';


    p_bit_cntr:
    process(i_clk)
    begin
      if rising_edge(i_clk) then
        if state = STANDBY or next_state = STANDBY then
          bit_cntr <= unsigned('1' & i_cfg_charsize);
        elsif state = RX or state = STOP then
          if sample = '1' then
            if bit_cntr = 0 then
              bit_cntr <= "00" & i_cfg_stop_bits;
            else
              bit_cntr <= bit_cntr - 1;
            end if;
          end if;
        end if;
      end if;
    end process;


    p_state_comb:
    process(state, rx_bit, bit_cntr, sample, rx_bit_ff, buf)
    begin
      next_state <= state;

      case state is
        when STANDBY =>
          if rx_bit_ff = '1' and rx_bit = '0' then
            next_state <= START;
          end if;

        when START =>
          if sample = '1' then
            if rx_bit_ff = '0' then
              next_state <= RX;
            else
              next_state <= STANDBY;
            end if;
          end if;

        when RX =>
          if sample = '1' then
            if bit_cntr = 0 then
              if i_cfg_parity_en = '1' then
                next_state <= PARITY;
              else
                next_state <= STOP;
              end if;
            end if;
          end if;

        when STOP =>
          if sample = '1' then
            if bit_cntr = 0 then
              if rx_bit_ff = '0' then
                next_state <= BREAK;
              else
                if buf.full = '1' then
                  next_state <= OVERRUN;
                else
                  next_state <= STANDBY;
                end if;
              end if;
            end if;
          end if;

        when PARITY =>
          if sample = '1' then
            next_state <= STOP;
          end if;

        when BREAK =>
          if sample = '1' and rx_bit_ff = '1' then
            next_state <= STANDBY;
          end if;

        when OVERRUN =>
          if buf.rd = '1' then
            next_state <= STANDBY;
          end if;

        when others =>
          next_state <= STANDBY;
      end case;
    end process;


    p_state_reg: process(i_clk)
    begin
      if rising_edge(i_clk) then
        if i_rst = RST_VALUE then
          state <= STANDBY;
        else
          state <= next_state;
        end if;
      end if;
    end process;


    p_err:
    process(i_clk)
    begin
      if rising_edge(i_clk) then
        if i_rst = '1' then
          o_err_parity  <= '0';
          err_framing <= '0';

        elsif sample = '1' then
          case state is
            when PARITY =>
              if rx_bit_ff /= parity_bit then
                o_err_parity <= '1';
              else
                o_err_parity <= '0';
              end if;

            when STOP =>
              if i_cfg_stop_bits = '1' and bit_cntr = 0 then
                err_framing <= (not rx_bit_ff) or err_framing;
              else
                err_framing <= not rx_bit_ff;
              end if;
            when others =>
          end case;
        end if;
      end if;
    end process;

    o_err_overrun <= '1' when state = OVERRUN else '0';

    o_err_framing <= err_framing;


    p_parity_bit:
    process(i_clk)
    begin
      if rising_edge(i_clk) then
        if i_rst = RST_VALUE then
          parity_bit <= i_cfg_parity_type;
        elsif i_cfg_parity_en = '1' then
          if state = RX and sample = '1' then
            parity_bit <= parity_bit xor rx_bit_ff;
          elsif state = STANDBY then
            parity_bit <= i_cfg_parity_type;
          end if;
        else
          parity_bit <= i_cfg_parity_type;
        end if;
      end if;
    end process;


    p_rx_shiftreg:
    process(i_clk)
    begin
      if rising_edge(i_clk) then
        if i_rst = RST_VALUE then
          shift_reg <= (others => '0');
        elsif state = RX and sample = '1' then
          shift_reg <= rx_bit_ff & shift_reg(shift_reg'left downto shift_reg'right+1);
        end if;
      end if;
    end process;


    buf.rd <= i_rx_re;
    rx_val <= std_logic_vector(shift_right(unsigned(shift_reg), to_integer(unsigned(not i_cfg_charsize))));

    p_buf_reg:
    process(i_clk)
    begin
      if rising_edge(i_clk) then
        if i_rst = RST_VALUE then
          buf.full <= '0';
          buf.data <= (others => '0');
        else
          if buf.wr = '1' and buf.full = '0' then
            buf.data <= rx_val;
            buf.full <= '1';
          elsif buf.wr = '1' and buf.full = '1' and buf.rd = '1' then
            buf.data <= rx_val;
          elsif buf.wr = '0' and buf.full = '1' and buf.rd = '1' then
            buf.full <= '0';
          end if;
        end if;
      end if;

    end process;

    buf.wr <= '1' when (state = STOP and sample = '1' and bit_cntr = 0) else
              '1' when state = OVERRUN else
              '0';


    o_rx_data   <= buf.data;
    o_rx_data_rdy <= buf.full;

  end block;

  ----------------------------------------------------------------------

  b_tx:
  block
    type tx_state_t is (START, TX, STOP, PARITY);
    signal state, next_state : tx_state_t;

    signal oversample_cntr : unsigned(i_cfg_baudrate'range);
    signal oversample_clken: std_logic;

    signal baud_cntr  : unsigned(3 downto 0);
    signal send_bit   : std_logic;

    signal bit_cntr   : unsigned(2 downto 0);
    signal tx_bit     : std_logic;

    signal shift_reg  : std_logic_vector(7 downto 0);

    signal parity_bit  : std_logic;

    type reg_t is record
      data    : std_logic_vector(7 downto 0);
      empty   : std_logic;
      rd, wr  : std_logic;
    end record;
    signal buf : reg_t;

  begin

    buf.wr <= i_tx_we;
    buf.rd <= '1' when state = START else '0';

    p_tx_reg:
    process(i_clk)
    begin
      if rising_edge(i_clk) then
        if i_rst = RST_VALUE then
          buf.empty <= '1';
          buf.data  <= (others => '0');
        elsif buf.wr = '1' and buf.empty = '1' and buf.rd = '0' then
          buf.data <= i_tx_data;
          buf.empty <= '0';
        elsif buf.wr = '1' and buf.empty = '0' and buf.rd = '1' then
          buf.data <= i_tx_data;
        elsif buf.wr = '0' and buf.empty = '0' and buf.rd = '1' then
          buf.empty <= '1';
        end if;
      end if;
    end process;


    -- FIXME: Sim problem... sync i think
    p_oversample_cntr:
    process(i_clk)
    begin
      if rising_edge(i_clk) then
        if i_rst = RST_VALUE or oversample_clken = '1' then
          oversample_cntr <= unsigned(i_cfg_baudrate);
        else
          oversample_cntr <= oversample_cntr - 1;
        end if;
      end if;
    end process;

    oversample_clken <= '1' when oversample_cntr = 0 else
                        '0';


    p_baudcntr:
    process(i_clk)
    begin
      if rising_edge(i_clk) then
        if i_rst = RST_VALUE then
          baud_cntr <= (others => '0');
        elsif oversample_clken = '1' then
          if baud_cntr /= 0 then
            baud_cntr <= baud_cntr - 1;
          elsif state /= stop or buf.empty = '0' or bit_cntr /= 0 then
            baud_cntr <= (others => '1');
          end if;
        end if;
      end if;
    end process;

    send_bit <= '1' when (state /= stop and baud_cntr = 0 and oversample_clken = '1') else
                '1' when (state = stop and baud_cntr = 0 and oversample_clken = '1' and buf.empty = '0' and bit_cntr = 0 ) else
                '0';


    p_bit_cntr:
    process(i_clk)
    begin
      if rising_edge(i_clk) then
        if i_rst = RST_VALUE then
          bit_cntr <= (others => '0');
        else
          case state is
            when TX =>
              if send_bit = '1' then
                if bit_cntr /= 0 then
                  bit_cntr <= bit_cntr - 1;
                else
                  bit_cntr <= "00" & i_cfg_stop_bits;
                end if;
              end if;

            when START =>
              bit_cntr <= unsigned('1' & i_cfg_charsize);

            when STOP =>
              if bit_cntr /= 0 then
                bit_cntr <= bit_cntr - 1;
              end if;

            when others =>
          end case;
        end if;
      end if;
    end process;


    p_state_comb:
    process(state, bit_cntr, i_cfg_parity_en, buf)
    begin
      next_state  <= state;

      case state is
        when STOP =>
          if bit_cntr = 0 then
            if buf.empty = '0' then
              next_state <= START;
            end if;
          end if;

        when START =>
          next_state <= TX;

        when TX =>
          if bit_cntr = 0 then
            if i_cfg_parity_en = '1' then
              next_state <= PARITY;
            else
              next_state <= STOP;
            end if;
          end if;

        when PARITY =>
          next_state <= STOP;

        when others =>
          next_state <= STOP;
      end case;
    end process;


    p_state_reg:
    process(i_clk)
    begin
      if rising_edge(i_clk) then
        if i_rst = RST_VALUE then
          state <= STOP;
        elsif send_bit = '1' then
          state <= next_state;
        end if;
      end if;
    end process;

    --tx_val <=

    p_shiftreg:
    process(i_clk)
    begin
      if rising_edge(i_clk) then
        if send_bit = '1' then
          if state = START then
            shift_reg <= buf.data;
          elsif state = TX then
            shift_reg <= '0' & shift_reg(shift_reg'left downto shift_reg'right+1);
          end if;
        end if;
      end if;
    end process;


    p_tx_bit:
    with state select
      tx_bit <= '1'          when STOP,
                '0'          when START,
                shift_reg(0) when TX,
                parity_bit   when PARITY,
                '1'          when others;


    p_parity_bit:
    process(i_clk)
    begin
      if rising_edge(i_clk) then
        if i_rst = RST_VALUE or i_cfg_parity_en = '0' then
          parity_bit <= i_cfg_parity_type;
        else
          if state = TX then
            if send_bit = '1' then
              parity_bit <= parity_bit xor tx_bit;
            end if;
          else
            parity_bit <= i_cfg_parity_type;
          end if;
        end if;
      end if;
    end process;


    o_err_underrun <= '1' when buf.empty = '1' and state = STOP else '0';


    process(i_clk)
    begin
      if rising_edge(i_clk) then
        if i_rst = RST_VALUE then
          o_tx <= '1';
        else
          o_tx <= tx_bit;
        end if;
      end if;
    end process;

    o_tx_data_rdy <= buf.empty;

  end block;

end architecture;
