library ieee;
use ieee.std_logic_1164.all;

package tb_functions is
  procedure clk_gen(signal clk      : out std_logic;
                    constant FREQ   : real;
                    constant PHASE  : time := 0 fs);
end package;

package body tb_functions is

  --! @fn Procedure clk_gen
  --! @param clk the clock signal to drive
  --! @param FREQ the frequency of the driven clock signal
  --! @param PHASE Initial phase of the clock
  --! @brief Generates a clock signal of FREQ
  procedure clk_gen(signal clk      : out std_logic;
                    constant FREQ   : real;
                    constant PHASE  : time := 0 fs) is
    constant PERIOD    : time := 1 sec / FREQ;        -- Full period
    constant HIGH_TIME : time := PERIOD / 2;          -- High time
    constant LOW_TIME  : time := PERIOD - HIGH_TIME;  -- Low time; always >= HIGH_TIME
  begin
    -- Check the arguments
    assert (HIGH_TIME /= 0 fs)
    report "clk_gen: High time is zero; time resolution to large for frequency"
    severity FAILURE;

    -- Clock generator
    clk <= '0';
    wait for PHASE;

    loop
      clk <= '1';
      wait for HIGH_TIME;
      clk <= '0';
      wait for LOW_TIME;
    end loop;
  end procedure;

end package body;

library ieee;
use ieee.std_logic_1164.all;

package uart_cfg is

  constant STOPBIT_1     : std_logic := '0';
  constant STOPBIT_2     : std_logic := '1';

  constant CHARSIZE_5BIT : std_logic_vector(1 downto 0) := "00";
  constant CHARSIZE_6BIT : std_logic_vector(1 downto 0) := "01";
  constant CHARSIZE_7BIT : std_logic_vector(1 downto 0) := "10";
  constant CHARSIZE_8BIT : std_logic_vector(1 downto 0) := "11";

  constant PARITY_ODD   : std_logic := '1';
  constant PARITY_EVEN  : std_logic := '0';

  constant AUTOBAUD_1X  : std_logic_vector(1 downto 0) := "00";
  constant AUTOBAUD_10X : std_logic_vector(1 downto 0) := "01";
  constant AUTOBAUD_7F  : std_logic_vector(1 downto 0) := "10";
  constant AUTOBAUD_55  : std_logic_vector(1 downto 0) := "11";

end package;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library vunit_lib;
context vunit_lib.vunit_context;
context vunit_lib.com_context;

use vunit_lib.uart_pkg.all;
use vunit_lib.stream_master_pkg.all;
use vunit_lib.stream_slave_pkg.all;

use work.tb_functions.all;
use work.uart_cfg.all;

entity uart_tb is
  generic(runner_cfg : string);
end entity;

architecture tb of uart_tb is

  -- DUT
  signal clk_i           : std_logic := '0';
  signal rst_i           : std_logic := '1';
  signal rx_rd_i         : std_logic := '0';
  signal tx_wr_i         : std_logic := '0';
  signal tx_i            : std_logic_vector(7 downto 0) := (others => '0');
  signal rxdr_o          : std_logic := '0';
  signal rx_o            : std_logic_vector(7 downto 0) := (others => '0');
  signal parity_err_o    : std_logic := '0';
  signal framing_err_o   : std_logic := '0';
  signal overrun_err_o   : std_logic := '0';
  signal underrun_err_o  : std_logic := '0';
  signal txbe_o          : std_logic := '0';
  signal en_i            : std_logic := '0';
  signal charsize_i      : std_logic_vector(1 downto 0) := (others => '0');
  signal autobaud_en_i   : std_logic := '0';
  signal autobaud_cfg_i  : std_logic_vector(1 downto 0) := (others => '0');
  signal parity_en_i     : std_logic := '0';
  signal parity_type_i   : std_logic := '0';
  signal stop_bits_i     : std_logic := '0';
  signal baudrate_gen_i  : std_logic_vector(15 downto 0) := (others => '0');
  signal tx_o            : std_logic := '1';
  signal rx_i            : std_logic := '1';

  constant FCLK       : natural := 14_745_600;
  constant OVERSAMPLE : natural := 16;

  -- VERIFICATION COMPONENT
  signal master_uart   : uart_master_t   := new_uart_master;
  signal master_stream : stream_master_t := as_stream(master_uart);
  signal slave_uart    : uart_slave_t    := new_uart_slave;
  signal slave_stream  : stream_slave_t  := as_stream(slave_uart);

begin

  clk_gen(clk_i, real(FCLK));
  test_runner_watchdog(runner, 50 ms);

  main:
  process
    variable baudrate : natural;
    variable char, exp_char : std_logic_vector(7 downto 0);

    procedure uart_echo(c : std_logic_vector(7 downto 0)) is
    begin
      push_stream(net, master_stream, c);

      while rxdr_o = '0' loop
        wait until rising_edge(clk_i);
      end loop;

        rx_rd_i <= '1';
        wait until rising_edge(clk_i);

        rx_rd_i <= '0';
        tx_wr_i <= '1';
        tx_i    <= rx_o;
        wait until rising_edge(clk_i);

        tx_wr_i <= '0';
        wait until rising_edge(clk_i);

      check_stream(net, slave_stream, c);
    end procedure;

  begin
    test_runner_setup(runner, runner_cfg);

    while test_suite loop
      if run("echo_default") then
        wait until rising_edge(clk_i);
        -- Config
        en_i           <= '1';
        charsize_i     <= CHARSIZE_8BIT;
        stop_bits_i    <= STOPBIT_1;
        baudrate       := 9600;
        baudrate_gen_i <= std_logic_vector(to_unsigned(FCLK / (OVERSAMPLE * baudrate) - 1, baudrate_gen_i'length));
        set_baud_rate(net, master_uart, baudrate);
        set_baud_rate(net, slave_uart, baudrate);

        wait until rising_edge(clk_i);
        rst_i <= '0';
        wait until rising_edge(clk_i);

        uart_echo(x"6A");
        check(framing_err_o = '0');

      elsif run("echo_2stop") then
        wait until rising_edge(clk_i);
        -- Config
        en_i           <= '1';
        charsize_i     <= CHARSIZE_8BIT;
        stop_bits_i    <= STOPBIT_2;
        baudrate       := 115200;
        baudrate_gen_i <= std_logic_vector(to_unsigned(FCLK / (OVERSAMPLE * baudrate) - 1, baudrate_gen_i'length));
        set_baud_rate(net, master_uart, baudrate);
        set_baud_rate(net, slave_uart, baudrate);

        wait until rising_edge(clk_i);
        rst_i <= '0';
        wait until rising_edge(clk_i);

        uart_echo(x"7B");
        check(framing_err_o = '0');

      elsif run("echo_max_baudrate") then
        en_i           <= '1';
        charsize_i     <= CHARSIZE_8BIT;
        stop_bits_i    <= STOPBIT_1;
        baudrate       := FCLK/OVERSAMPLE;
        baudrate_gen_i <= std_logic_vector(to_unsigned(FCLK / (OVERSAMPLE * baudrate) - 1, baudrate_gen_i'length));
        baudrate_gen_i <= (others => '0');
        set_baud_rate(net, master_uart, baudrate);
        set_baud_rate(net, slave_uart, baudrate);

        wait until rising_edge(clk_i);
        rst_i <= '0';
        wait until rising_edge(clk_i);

        uart_echo(x"96");
        check(framing_err_o = '0');

      elsif run("echo_7bit") then
        --cfg
        en_i           <= '1';
        charsize_i     <= CHARSIZE_7BIT;
        stop_bits_i    <= STOPBIT_1;
        baudrate       := 115200;
        baudrate_gen_i <= std_logic_vector(to_unsigned(FCLK / (OVERSAMPLE * baudrate) - 1, baudrate_gen_i'length));
        set_baud_rate(net, master_uart, baudrate);
        set_baud_rate(net, slave_uart, baudrate);

        wait until rising_edge(clk_i);
        rst_i <= '0';
        wait until rising_edge(clk_i);

        -- Vunit module is 8 bit but our DUT will expect 7 bit
        -- Therefore MSB = '1' to act like a stop bit
        -- [start][0][1][2]...[7][stop]
        char     := x"95";
        exp_char := x"15";
        push_stream(net, master_stream, char);

        while rxdr_o = '0' loop
          wait until rising_edge(clk_i);
        end loop;

        rx_rd_i <= '1';
        wait until rising_edge(clk_i);

        rx_rd_i <= '0';
        tx_wr_i <= '1';
        tx_i    <= rx_o;
        wait until rising_edge(clk_i);

        tx_wr_i <= '0';
        wait until rising_edge(clk_i);

        check_equal(tx_i, exp_char);

        check(framing_err_o = '0');

        tx_wr_i <= '0';
        rx_rd_i <= '0';
        wait until rising_edge(clk_i);

        check_stream(net, slave_stream, char);

      elsif run("echo_parity_odd") then
        --cfg
        -- Vunit module is 8 bit, so set the module to 7 bit + parity=8 bit
        -- Therefore MSB = '1' to act like a stop bit
        -- [start][0][1][2]...[7][P][stop]
        en_i           <= '1';
        charsize_i     <= CHARSIZE_7BIT;
        stop_bits_i    <= STOPBIT_1;
        baudrate       := 115200;
        baudrate_gen_i <= std_logic_vector(to_unsigned(FCLK / (OVERSAMPLE * baudrate) - 1, baudrate_gen_i'length));
        parity_en_i    <= '1';
        parity_type_i  <= PARITY_ODD;

        set_baud_rate(net, master_uart, baudrate);
        set_baud_rate(net, slave_uart, baudrate);

        wait until rising_edge(clk_i);
        rst_i <= '0';
        wait until rising_edge(clk_i);

        -- The number of '1' must be ODD
        -- Parity bit is the MSB
        char     := b"1_101_0011";
        push_stream(net, master_stream, char);

        while rxdr_o = '0' loop
          wait until rising_edge(clk_i);
        end loop;

        check(parity_err_o = '0');

        rx_rd_i <= '1';
        wait until rising_edge(clk_i);

        rx_rd_i <= '0';
        tx_wr_i <= '1';
        tx_i    <= rx_o;
        wait until rising_edge(clk_i);

        tx_wr_i <= '0';
        wait until rising_edge(clk_i);

        exp_char := x"53";
        check_equal(tx_i, exp_char);

        check(parity_err_o = '0');
        check(framing_err_o = '0');

        tx_wr_i <= '0';
        rx_rd_i <= '0';
        wait until rising_edge(clk_i);

        check_stream(net, slave_stream, char);

      elsif run("echo_parity_even_err") then
        en_i           <= '1';
        charsize_i     <= CHARSIZE_7BIT;
        stop_bits_i    <= STOPBIT_1;
        baudrate       := 115200;
        baudrate_gen_i <= std_logic_vector(to_unsigned(FCLK / (OVERSAMPLE * baudrate) - 1, baudrate_gen_i'length));
        parity_en_i    <= '1';
        parity_type_i  <= PARITY_EVEN;

        set_baud_rate(net, master_uart, baudrate);
        set_baud_rate(net, slave_uart, baudrate);

        wait until rising_edge(clk_i);
        rst_i <= '0';
        wait until rising_edge(clk_i);

        -- The number of '1' must be even
        -- Parity bit is the MSB
        char     := b"1_101_0011";
        push_stream(net, master_stream, char);

        while rxdr_o = '0' loop
          wait until rising_edge(clk_i);
        end loop;

        rx_rd_i <= '1';
        wait until rising_edge(clk_i);

        rx_rd_i <= '0';
        tx_wr_i <= '1';
        tx_i    <= rx_o;
        wait until rising_edge(clk_i);

        tx_wr_i <= '0';
        wait until rising_edge(clk_i);

        exp_char := x"53";
        check_equal(tx_i, exp_char);

        check(framing_err_o = '0');

        tx_wr_i <= '0';
        rx_rd_i <= '0';
        wait until rising_edge(clk_i);

        -- Should not change until overwritten by next byte
        check(parity_err_o = '1');

        -- Real parity bit should be '0' therefore: "0_101_0011
        check_stream(net, slave_stream, exp_char);

     elsif run("framing_err_1bit") then
        -- Vunit uart=8 bit, set to uart to 7 bit to control stop bit
        en_i           <= '1';
        charsize_i     <= CHARSIZE_7BIT;
        stop_bits_i    <= STOPBIT_1;
        baudrate       := 115200;
        baudrate_gen_i <= std_logic_vector(to_unsigned(FCLK / (OVERSAMPLE * baudrate) - 1, baudrate_gen_i'length));
        set_baud_rate(net, master_uart, baudrate);
        set_baud_rate(net, slave_uart, baudrate);

        wait until rising_edge(clk_i);
        rst_i <= '0';
        wait until rising_edge(clk_i);

        -- msb -> stop bit
        char := b"0_110_0101";
        push_stream(net, master_stream, char);

        while rxdr_o = '0' loop
          wait until rising_edge(clk_i);
        end loop;

        rx_rd_i <= '1';
        wait until rising_edge(clk_i);

        rx_rd_i <= '0';
        tx_wr_i <= '1';
        tx_i    <= rx_o;
        wait until rising_edge(clk_i);

        tx_wr_i <= '0';
        wait until rising_edge(clk_i);
        -- Add a '1' for simulated stop bit
        exp_char := x"65";
        check_equal(tx_i, exp_char);

        check(framing_err_o = '1');


        check(framing_err_o = '1');

        exp_char := x"E5";
        check_stream(net, slave_stream, exp_char);

        -- Clear error by sending new valid frame
        char := b"1_110_0101";
        push_stream(net, master_stream, char);

        while rxdr_o = '0' loop
          wait until rising_edge(clk_i);
        end loop;

        check(framing_err_o = '0');

      elsif run("framing_err_2bit") then
        -- Vunit uart=8 bit, set to uart to 6 bit to control the 2 stop bit
        en_i           <= '1';
        charsize_i     <= CHARSIZE_6BIT;
        stop_bits_i    <= STOPBIT_2;
        baudrate       := 115200;
        baudrate_gen_i <= std_logic_vector(to_unsigned(FCLK / (OVERSAMPLE * baudrate) - 1, baudrate_gen_i'length));
        set_baud_rate(net, master_uart, baudrate);
        set_baud_rate(net, slave_uart, baudrate);

        wait until rising_edge(clk_i);
        rst_i <= '0';
        wait until rising_edge(clk_i);

        -- [stop2][stop1][data][start]
        char := b"10_10_1010";
        push_stream(net, master_stream, char);

        while rxdr_o = '0' loop
          wait until rising_edge(clk_i);
        end loop;

        rx_rd_i <= '1';
        wait until rising_edge(clk_i);

        rx_rd_i <= '0';
        tx_wr_i <= '1';
        tx_i    <= rx_o;
        wait until rising_edge(clk_i);

        tx_wr_i <= '0';
        wait until rising_edge(clk_i);

        -- Add a '1' for simulated stop bit
        exp_char := x"2A";
        check_equal(tx_i, exp_char);

        check(framing_err_o = '1');


        check(framing_err_o = '1');

        exp_char := x"EA";
        check_stream(net, slave_stream, exp_char);

        -- Clear error by sending new valid frame
        char := b"11_10_1010";
        push_stream(net, master_stream, char);

        while rxdr_o = '0' loop
          wait until rising_edge(clk_i);
        end loop;

        check(framing_err_o = '0');

     elsif run("overrun_err") then
        -- Config
        en_i           <= '1';
        charsize_i     <= CHARSIZE_8BIT;
        stop_bits_i    <= STOPBIT_1;
        baudrate       := 115200;
        baudrate_gen_i <= std_logic_vector(to_unsigned(FCLK / (OVERSAMPLE * baudrate) - 1, baudrate_gen_i'length));
        set_baud_rate(net, master_uart, baudrate);
        set_baud_rate(net, slave_uart, baudrate);

        wait until rising_edge(clk_i);
        rst_i <= '0';
        wait until rising_edge(clk_i);

        char := x"AA";
        push_stream(net, master_stream, char);

        wait until rxdr_o = '1';
        check(overrun_err_o = '0', "Overrun /= '0'");

        -- Should be lost data
        char := x"BB";
        push_stream(net, master_stream, char);

        char := x"CC";
        push_stream(net, master_stream, char);

        wait for 1 sec * (real(10)/real(baudrate));
        check(overrun_err_o = '1', "Overrun /= '1'");
        wait for 1 sec * (real(10)/real(baudrate));

        rx_rd_i <= '1';
        wait until rising_edge(clk_i);

        tx_wr_i <= '1';
        tx_i    <= rx_o;
        wait until rising_edge(clk_i);

        exp_char := x"AA";
        check_equal(tx_i, exp_char);
        tx_i    <= rx_o;
        wait until rising_edge(clk_i);

        -- x"CC" will not be pushed in the regfile
        exp_char := x"BB";
        check_equal(tx_i, exp_char);

        tx_wr_i <= '0';
        rx_rd_i <= '0';
        wait until rising_edge(clk_i);

        check(rxdr_o = '0', "All data should have been read");

     -- elsif run("autobaud_mode1") then
     -- elsif run("autobaud_mode10") then
     -- elsif run("autobaud_mode7F") then
     -- elsif run("autobaud_mode55") then

      end if;
    end loop;

    test_runner_cleanup(runner);
  end process;


  DUT: entity work.uart(rtl)
  generic map(
    FCLK      => FCLK,
    RST_VALUE => '1'
  )
  port map (
    clk_i           => clk_i,
    rst_i           => rst_i,
    rx_rd_i         => rx_rd_i,
    tx_wr_i         => tx_wr_i,
    rxdr_o          => rxdr_o,
    tx_i            => tx_i,
    rx_o            => rx_o,
    parity_err_o    => parity_err_o,
    framing_err_o   => framing_err_o,
    overrun_err_o   => overrun_err_o,
    underrun_err_o  => underrun_err_o,
    txbe_o          => txbe_o,
    en_i            => en_i,
    charsize_i      => charsize_i,
    autobaud_en_i   => autobaud_en_i,
    autobaud_cfg_i  => autobaud_cfg_i,
    parity_en_i     => parity_en_i,
    parity_type_i   => parity_type_i,
    stop_bits_i     => stop_bits_i,
    baudrate_gen_i  => baudrate_gen_i,
    tx_o            => tx_o,
    rx_i            => rx_i
  );


  uart_master_inst : entity vunit_lib.uart_master
    generic map(
      uart => master_uart
    )
    port map(
      tx => rx_i
    );


  uart_slave_inst : entity vunit_lib.uart_slave
    generic map(
      uart => slave_uart
    )
    port map(
      rx => tx_o
    );

end architecture;

