library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package uart_pkg is
end package;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity uart is
  generic (
    FCLK         : natural := 14_745_600; -- Value specified must be at least 16 time the maximum supported baudrate
    RST_VALUE    : std_logic
  );
  port (
    -- INTERNAL interface
    clk_i           : in  std_logic;
    rst_i           : in  std_logic;
    -- UART PHY interface
    tx_o            : out std_logic;
    rx_i            : in  std_logic;
    -- Read and Write for data registers
    rx_rd_i         : in  std_logic;
    tx_wr_i         : in  std_logic;
    -- TXDR Transmit Register
    tx_i            : in  std_logic_vector(7 downto 0); -- Transmit data
    -- RXDR Receive Register
    rx_o            : out std_logic_vector(7 downto 0); -- Received data
    -- LSR Line Status Register
    rxdr_o          : out std_logic;                    -- RX Data Ready
    parity_err_o    : out std_logic;                    -- Receiver detected that data doesnt match parity bit
    framing_err_o   : out std_logic;                    -- Receiver did not see the stop bit at expected stop time
    overrun_err_o   : out std_logic;                    -- Receiver buffer full and tried to write a new data (new data lost)
    underrun_err_o  : out std_logic;                    -- TX buffer & TX Shift reg empty
    txbe_o          : out std_logic;                    -- TX buffer empty
    -- CTR1 ConTrol Register 1
    en_i            : in  std_logic;                     -- Global Uart Enable : Not implemented
    charsize_i      : in  std_logic_vector(1 downto 0);  -- Character size  5(00),6(01),7(10),8(11)
    parity_en_i     : in  std_logic;                     -- Parity bit enable: 0 disable, 1 enable
    parity_type_i   : in  std_logic;                     -- Party type: Even (0), Odd (1)
    stop_bits_i     : in  std_logic;                     -- Number of stop bits 1('0'), 2('1')
    -- CTR2 Control Register 2
    autobaud_en_i   : in  std_logic;                     -- Autobaud enable : Not implemented
    autobaud_cfg_i  : in  std_logic_vector(1 downto 0);  -- Autobaud mode -- 00: 01X mode, 01: 010X mode, 10: 0x7F mode, 11: 0x55 mode, 10 & 11 mode unsuported when charsize /= 11
    -- FCR Fifo Control Register : Not implemented
    -- BRHR & BRLR Baud Rate High and Low registers
    baudrate_gen_i  : in  std_logic_vector(15 downto 0)  -- Baud rate generator value: FCLK/(16*BAUD) - 1
  );
end entity uart;

architecture rtl of uart is

  constant OVERSAMPLE : natural := 16;

  -- TODO: Fifo

begin

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

    signal rx_bit     : std_logic;
    signal parity_bit : std_logic;
    signal bit_cntr   : unsigned(2 downto 0); -- 0 to 7 encoded

    signal oversample_cntr : unsigned(baudrate_gen_i'range);
    signal oversample_clken : std_logic;

    signal baud_cntr : unsigned(3 downto 0);
    signal sample     : std_logic;

    signal shift_reg  : std_logic_vector(7 downto 0);
    signal rx_val     : std_logic_vector(7 downto 0);

  begin

    rx_bit <= rx_i when rising_edge(clk_i);


    p_oversample_cntr:
    process(clk_i)
    begin
      if rising_edge(clk_i) then
        if state = STANDBY then
          if next_state = START and unsigned(baudrate_gen_i) /= 0 then
            oversample_cntr <= oversample_cntr - 1;
          else
            oversample_cntr <= unsigned(baudrate_gen_i);
          end if;
        elsif oversample_clken = '1' then
          oversample_cntr <= unsigned(baudrate_gen_i);
        else
          oversample_cntr <= oversample_cntr - 1;
        end if;
      end if;
    end process;

    oversample_clken <= '1' when oversample_cntr = 0 else '0';


    p_baud_cntr:
    process(clk_i)
    begin
      if rising_edge(clk_i) then
        if rst_i = RST_VALUE or state = STANDBY then
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
    process(clk_i)
    begin
      if rising_edge(clk_i) then
        if state = STANDBY or next_state = STANDBY then
          bit_cntr <= unsigned('1' & charsize_i);
        elsif state = RX or state = STOP then
          if sample = '1' then
            if bit_cntr = 0 then
              bit_cntr <= "00" & stop_bits_i;
            else
              bit_cntr <= bit_cntr - 1;
            end if;
          end if;
        end if;
      end if;
    end process;


    p_state_comb:
    process(state, rx_i, bit_cntr, sample, rx_bit, buf)
    begin
      next_state <= state;

      case state is
        when STANDBY =>
          if rx_bit = '1' and rx_i = '0' then
            next_state <= START;
          end if;

        when START =>
          if sample = '1' then
            if rx_bit = '0' then
              next_state <= RX;
            else
              next_state <= STANDBY;
            end if;
          end if;

        when RX =>
          if sample = '1' then
            if bit_cntr = 0 then
              if parity_en_i = '1' then
                next_state <= PARITY;
              else
                next_state <= STOP;
              end if;
            end if;
          end if;

        when STOP =>
          if sample = '1' then
            if bit_cntr = 0 then
              if rx_bit = '0' then
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
          if sample = '1' and rx_bit = '1' then
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


    p_state_reg: process(clk_i)
    begin
      if rising_edge(clk_i) then
        if rst_i = RST_VALUE then
          state <= STANDBY;
        else
          state <= next_state;
        end if;
      end if;
    end process;


    p_err:
    process(clk_i)
    begin
      if rising_edge(clk_i) then
        if rst_i = '1' then
          parity_err_o  <= '0';
          framing_err_o <= '0';

        elsif sample = '1' then
          case state is
            when PARITY =>
              if rx_bit /= parity_bit then
                parity_err_o <= '1';
              else
                parity_err_o <= '0';
              end if;

            when STOP =>
              if stop_bits_i = '1' and bit_cntr = 0 then
                framing_err_o <= (not rx_bit) or framing_err_o;
              else
                framing_err_o <= not rx_bit;
              end if;
            when others =>
          end case;
        end if;
      end if;
    end process;

    overrun_err_o <= '1' when state = OVERRUN else '0';


    p_parity_bit:
    process(clk_i)
    begin
      if rising_edge(clk_i) then
        if rst_i = RST_VALUE then
          parity_bit <= parity_type_i;
        elsif parity_en_i = '1' then
          if state = RX and sample = '1' then
            parity_bit <= parity_bit xor rx_bit;
          elsif state = STANDBY then
            parity_bit <= parity_type_i;
          end if;
        else
          parity_bit <= parity_type_i;
        end if;
      end if;
    end process;


    p_rx_shiftreg:
    process(clk_i)
    begin
      if rising_edge(clk_i) then
        if rst_i = RST_VALUE then
          shift_reg <= (others => '0');
        elsif state = RX and sample = '1' then
          shift_reg <= rx_bit & shift_reg(shift_reg'left downto shift_reg'right+1);
        end if;
      end if;
    end process;


    buf.rd <= rx_rd_i;
    rx_val <= std_logic_vector(shift_right(unsigned(shift_reg), to_integer(unsigned(not charsize_i))));

    p_buf_reg:
    process(clk_i)
    begin
      if rising_edge(clk_i) then
        if rst_i = RST_VALUE then
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


    rx_o   <= buf.data;
    rxdr_o <= buf.full;

  end block;

  ----------------------------------------------------------------------

  b_tx:
  block
    type tx_state_t is (START, TX, STOP, PARITY);
    signal state, next_state : tx_state_t;

    signal oversample_cntr : unsigned(baudrate_gen_i'range);
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

    buf.wr <= tx_wr_i;
    buf.rd <= '1' when state = START else '0';

    p_tx_reg:
    process(clk_i)
    begin
      if rising_edge(clk_i) then
        if rst_i = RST_VALUE then
          buf.empty <= '1';
          buf.data  <= (others => '0');
        elsif buf.wr = '1' and buf.empty = '1' and buf.rd = '0' then
          buf.data <= tx_i;
          buf.empty <= '0';
        elsif buf.wr = '1' and buf.empty = '0' and buf.rd = '1' then
          buf.data <= tx_i;
        elsif buf.wr = '0' and buf.empty = '0' and buf.rd = '1' then
          buf.empty <= '1';
        end if;
      end if;
    end process;


    -- FIXME: Sim problem... sync i think
    p_oversample_cntr:
    process(clk_i)
    begin
      if rising_edge(clk_i) then
        if rst_i = RST_VALUE or oversample_clken = '1' then
          oversample_cntr <= unsigned(baudrate_gen_i);
        else
          oversample_cntr <= oversample_cntr - 1;
        end if;
      end if;
    end process;

    oversample_clken <= '1' when oversample_cntr = 0 else
                        '0';


    p_baudcntr:
    process(clk_i)
    begin
      if rising_edge(clk_i) then
        if rst_i = RST_VALUE then
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
    process(clk_i)
    begin
      if rising_edge(clk_i) then
        if rst_i = RST_VALUE then
          bit_cntr <= (others => '0');
        else
          case state is
            when TX =>
              if send_bit = '1' then
                if bit_cntr /= 0 then
                  bit_cntr <= bit_cntr - 1;
                else
                  bit_cntr <= "00" & stop_bits_i;
                end if;
              end if;

            when START =>
              bit_cntr <= unsigned('1' & charsize_i);

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
    process(state, bit_cntr, parity_en_i, buf)
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
            if parity_en_i = '1' then
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
    process(clk_i)
    begin
      if rising_edge(clk_i) then
        if rst_i = RST_VALUE then
          state <= STOP;
        elsif send_bit = '1' then
          state <= next_state;
        end if;
      end if;
    end process;

    --tx_val <=

    p_shiftreg:
    process(clk_i)
    begin
      if rising_edge(clk_i) then
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
    process(clk_i)
    begin
      if rising_edge(clk_i) then
        if rst_i = RST_VALUE or parity_en_i = '0' then
          parity_bit <= parity_type_i;
        else
          if state = TX then
            if send_bit = '1' then
              parity_bit <= parity_bit xor tx_bit;
            end if;
          else
            parity_bit <= parity_type_i;
          end if;
        end if;
      end if;
    end process;


    underrun_err_o <= '1' when buf.empty = '1' and state = STOP else '0';


    process(clk_i)
    begin
      if rising_edge(clk_i) then
        if rst_i = RST_VALUE then
          tx_o <= '1';
        else
          tx_o <= tx_bit;
        end if;
      end if;
    end process;

    txbe_o <= buf.empty;

  end block;

end architecture;
