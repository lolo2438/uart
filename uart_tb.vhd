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
use work.uart_pkg.all;

entity uart_tb is
  generic(runner_cfg : string);
end entity;

architecture tb of uart_tb is

  -- DUT
  signal i_clk           : std_logic := '0';
  signal i_rst           : std_logic := '1';
  signal i_rx_re         : std_logic := '0';
  signal i_tx_we         : std_logic := '0';
  signal i_tx_data            : std_logic_vector(7 downto 0) := (others => '0');
  signal o_rx_data_rdy          : std_logic := '0';
  signal o_rx_data            : std_logic_vector(7 downto 0) := (others => '0');
  signal o_err_parity    : std_logic := '0';
  signal o_err_framing   : std_logic := '0';
  signal o_err_overrun   : std_logic := '0';
  signal o_err_underrun  : std_logic := '0';
  signal o_tx_data_rdy          : std_logic := '0';
  signal i_en            : std_logic := '0';
  signal i_cfg_charsize      : std_logic_vector(1 downto 0) := (others => '0');
  signal i_cfg_autobaud_en   : std_logic := '0';
  signal i_cfg_autobaud_mode  : std_logic_vector(1 downto 0) := (others => '0');
  signal i_cfg_parity_en     : std_logic := '0';
  signal i_cfg_parity_type   : std_logic := '0';
  signal i_cfg_stop_bits     : std_logic := '0';
  signal i_cfg_baudrate  : std_logic_vector(15 downto 0) := (others => '0');
  signal o_tx            : std_logic := '1';
  signal i_rx            : std_logic := '1';

  constant FCLK       : natural := 14_745_600;
  constant OVERSAMPLE : natural := 16;

  -- VERIFICATION COMPONENT
  signal master_uart   : uart_master_t   := new_uart_master;
  signal master_stream : stream_master_t := as_stream(master_uart);
  signal slave_uart    : uart_slave_t    := new_uart_slave;
  signal slave_stream  : stream_slave_t  := as_stream(slave_uart);

begin

  clk_gen(i_clk, real(FCLK));
  test_runner_watchdog(runner, 50 ms);

  main:
  process
    variable baudrate : natural;
    variable char, exp_char : std_logic_vector(7 downto 0);

    procedure uart_echo(c : std_logic_vector(7 downto 0)) is
    begin
      push_stream(net, master_stream, c);

      while o_rx_data_rdy = '0' loop
        wait until rising_edge(i_clk);
      end loop;

        i_rx_re <= '1';
        wait until rising_edge(i_clk);

        i_rx_re <= '0';
        i_tx_we <= '1';
        i_tx_data    <= o_rx_data;
        wait until rising_edge(i_clk);

        i_tx_we <= '0';
        wait until rising_edge(i_clk);

      check_stream(net, slave_stream, c);
    end procedure;

  begin
    test_runner_setup(runner, runner_cfg);

    while test_suite loop
      if run("echo_default") then
        wait until rising_edge(i_clk);
        -- Config
        i_en           <= '1';
        i_cfg_charsize     <= CFG_CHARSIZE_8BIT;
        i_cfg_stop_bits    <= CFG_STOP_BITS_1;
        baudrate       := 9600;
        i_cfg_baudrate <= std_logic_vector(to_unsigned(FCLK / (OVERSAMPLE * baudrate) - 1, i_cfg_baudrate'length));
        set_baud_rate(net, master_uart, baudrate);
        set_baud_rate(net, slave_uart, baudrate);

        wait until rising_edge(i_clk);
        i_rst <= '0';
        wait until rising_edge(i_clk);

        uart_echo(x"6A");
        check(o_err_framing = '0');

      elsif run("echo_2stop") then
        wait until rising_edge(i_clk);
        -- Config
        i_en           <= '1';
        i_cfg_charsize     <= CFG_CHARSIZE_8BIT;
        i_cfg_stop_bits    <= CFG_STOP_BITS_2;
        baudrate       := 115200;
        i_cfg_baudrate <= std_logic_vector(to_unsigned(FCLK / (OVERSAMPLE * baudrate) - 1, i_cfg_baudrate'length));
        set_baud_rate(net, master_uart, baudrate);
        set_baud_rate(net, slave_uart, baudrate);

        wait until rising_edge(i_clk);
        i_rst <= '0';
        wait until rising_edge(i_clk);

        uart_echo(x"7B");
        check(o_err_framing = '0');

      elsif run("echo_max_baudrate") then
        i_en           <= '1';
        i_cfg_charsize     <= CFG_CHARSIZE_8BIT;
        i_cfg_stop_bits    <= CFG_STOP_BITS_1;
        baudrate       := FCLK/OVERSAMPLE;
        i_cfg_baudrate <= std_logic_vector(to_unsigned(FCLK / (OVERSAMPLE * baudrate) - 1, i_cfg_baudrate'length));
        i_cfg_baudrate <= (others => '0');
        set_baud_rate(net, master_uart, baudrate);
        set_baud_rate(net, slave_uart, baudrate);

        wait until rising_edge(i_clk);
        i_rst <= '0';
        wait until rising_edge(i_clk);

        uart_echo(x"96");
        check(o_err_framing = '0');

      elsif run("echo_7bit") then
        --cfg
        i_en           <= '1';
        i_cfg_charsize     <= CFG_CHARSIZE_7BIT;
        i_cfg_stop_bits    <= CFG_STOP_BITS_1;
        baudrate       := 115200;
        i_cfg_baudrate <= std_logic_vector(to_unsigned(FCLK / (OVERSAMPLE * baudrate) - 1, i_cfg_baudrate'length));
        set_baud_rate(net, master_uart, baudrate);
        set_baud_rate(net, slave_uart, baudrate);

        wait until rising_edge(i_clk);
        i_rst <= '0';
        wait until rising_edge(i_clk);

        -- Vunit module is 8 bit but our DUT will expect 7 bit
        -- Therefore MSB = '1' to act like a stop bit
        -- [start][0][1][2]...[7][stop]
        char     := x"95";
        exp_char := x"15";
        push_stream(net, master_stream, char);

        while o_rx_data_rdy = '0' loop
          wait until rising_edge(i_clk);
        end loop;

        i_rx_re <= '1';
        wait until rising_edge(i_clk);

        i_rx_re <= '0';
        i_tx_we <= '1';
        i_tx_data    <= o_rx_data;
        wait until rising_edge(i_clk);

        i_tx_we <= '0';
        wait until rising_edge(i_clk);

        check_equal(i_tx_data, exp_char);

        check(o_err_framing = '0');

        i_tx_we <= '0';
        i_rx_re <= '0';
        wait until rising_edge(i_clk);

        check_stream(net, slave_stream, char);

      elsif run("echo_parity_odd") then
        --cfg
        -- Vunit module is 8 bit, so set the module to 7 bit + parity=8 bit
        -- Therefore MSB = '1' to act like a stop bit
        -- [start][0][1][2]...[7][P][stop]
        i_en           <= '1';
        i_cfg_charsize     <= CFG_CHARSIZE_7BIT;
        i_cfg_stop_bits    <= CFG_STOP_BITS_1;
        baudrate       := 115200;
        i_cfg_baudrate <= std_logic_vector(to_unsigned(FCLK / (OVERSAMPLE * baudrate) - 1, i_cfg_baudrate'length));
        i_cfg_parity_en    <= '1';
        i_cfg_parity_type  <= CFG_PARITY_TYPE_ODD;

        set_baud_rate(net, master_uart, baudrate);
        set_baud_rate(net, slave_uart, baudrate);

        wait until rising_edge(i_clk);
        i_rst <= '0';
        wait until rising_edge(i_clk);

        -- The number of '1' must be ODD
        -- Parity bit is the MSB
        char     := b"1_101_0011";
        push_stream(net, master_stream, char);

        while o_rx_data_rdy = '0' loop
          wait until rising_edge(i_clk);
        end loop;

        check(o_err_parity = '0');

        i_rx_re <= '1';
        wait until rising_edge(i_clk);

        i_rx_re <= '0';
        i_tx_we <= '1';
        i_tx_data    <= o_rx_data;
        wait until rising_edge(i_clk);

        i_tx_we <= '0';
        wait until rising_edge(i_clk);

        exp_char := x"53";
        check_equal(i_tx_data, exp_char);

        check(o_err_parity = '0');
        check(o_err_framing = '0');

        i_tx_we <= '0';
        i_rx_re <= '0';
        wait until rising_edge(i_clk);

        check_stream(net, slave_stream, char);

      elsif run("echo_parity_even_err") then
        i_en           <= '1';
        i_cfg_charsize     <= CFG_CHARSIZE_7BIT;
        i_cfg_stop_bits    <= CFG_STOP_BITS_1;
        baudrate       := 115200;
        i_cfg_baudrate <= std_logic_vector(to_unsigned(FCLK / (OVERSAMPLE * baudrate) - 1, i_cfg_baudrate'length));
        i_cfg_parity_en    <= '1';
        i_cfg_parity_type  <= CFG_PARITY_TYPE_EVEN;

        set_baud_rate(net, master_uart, baudrate);
        set_baud_rate(net, slave_uart, baudrate);

        wait until rising_edge(i_clk);
        i_rst <= '0';
        wait until rising_edge(i_clk);

        -- The number of '1' must be even
        -- Parity bit is the MSB
        char     := b"1_101_0011";
        push_stream(net, master_stream, char);

        while o_rx_data_rdy = '0' loop
          wait until rising_edge(i_clk);
        end loop;

        i_rx_re <= '1';
        wait until rising_edge(i_clk);

        i_rx_re <= '0';
        i_tx_we <= '1';
        i_tx_data    <= o_rx_data;
        wait until rising_edge(i_clk);

        i_tx_we <= '0';
        wait until rising_edge(i_clk);

        exp_char := x"53";
        check_equal(i_tx_data, exp_char);

        check(o_err_framing = '0');

        i_tx_we <= '0';
        i_rx_re <= '0';
        wait until rising_edge(i_clk);

        -- Should not change until overwritten by next byte
        check(o_err_parity = '1');

        -- Real parity bit should be '0' therefore: "0_101_0011
        check_stream(net, slave_stream, exp_char);

     elsif run("framing_err_1bit") then
        -- Vunit uart=8 bit, set to uart to 7 bit to control stop bit
        i_en           <= '1';
        i_cfg_charsize     <= CFG_CHARSIZE_7BIT;
        i_cfg_stop_bits    <= CFG_STOP_BITS_1;
        baudrate       := 115200;
        i_cfg_baudrate <= std_logic_vector(to_unsigned(FCLK / (OVERSAMPLE * baudrate) - 1, i_cfg_baudrate'length));
        set_baud_rate(net, master_uart, baudrate);
        set_baud_rate(net, slave_uart, baudrate);

        wait until rising_edge(i_clk);
        i_rst <= '0';
        wait until rising_edge(i_clk);

        -- msb -> stop bit
        char := b"0_110_0101";
        push_stream(net, master_stream, char);

        while o_rx_data_rdy = '0' loop
          wait until rising_edge(i_clk);
        end loop;

        i_rx_re <= '1';
        wait until rising_edge(i_clk);

        i_rx_re <= '0';
        i_tx_we <= '1';
        i_tx_data    <= o_rx_data;
        wait until rising_edge(i_clk);

        i_tx_we <= '0';
        wait until rising_edge(i_clk);
        -- Add a '1' for simulated stop bit
        exp_char := x"65";
        check_equal(i_tx_data, exp_char);

        check(o_err_framing = '1');


        check(o_err_framing = '1');

        exp_char := x"E5";
        check_stream(net, slave_stream, exp_char);

        -- Clear error by sending new valid frame
        char := b"1_110_0101";
        push_stream(net, master_stream, char);

        while o_rx_data_rdy = '0' loop
          wait until rising_edge(i_clk);
        end loop;

        check(o_err_framing = '0');

      elsif run("framing_err_2bit") then
        -- Vunit uart=8 bit, set to uart to 6 bit to control the 2 stop bit
        i_en           <= '1';
        i_cfg_charsize     <= CFG_CHARSIZE_6BIT;
        i_cfg_stop_bits    <= CFG_STOP_BITS_2;
        baudrate       := 115200;
        i_cfg_baudrate <= std_logic_vector(to_unsigned(FCLK / (OVERSAMPLE * baudrate) - 1, i_cfg_baudrate'length));
        set_baud_rate(net, master_uart, baudrate);
        set_baud_rate(net, slave_uart, baudrate);

        wait until rising_edge(i_clk);
        i_rst <= '0';
        wait until rising_edge(i_clk);

        -- [stop2][stop1][data][start]
        char := b"10_10_1010";
        push_stream(net, master_stream, char);

        while o_rx_data_rdy = '0' loop
          wait until rising_edge(i_clk);
        end loop;

        i_rx_re <= '1';
        wait until rising_edge(i_clk);

        i_rx_re <= '0';
        i_tx_we <= '1';
        i_tx_data    <= o_rx_data;
        wait until rising_edge(i_clk);

        i_tx_we <= '0';
        wait until rising_edge(i_clk);

        -- Add a '1' for simulated stop bit
        exp_char := x"2A";
        check_equal(i_tx_data, exp_char);

        check(o_err_framing = '1');


        check(o_err_framing = '1');

        exp_char := x"EA";
        check_stream(net, slave_stream, exp_char);

        -- Clear error by sending new valid frame
        char := b"11_10_1010";
        push_stream(net, master_stream, char);

        while o_rx_data_rdy = '0' loop
          wait until rising_edge(i_clk);
        end loop;

        check(o_err_framing = '0');

     elsif run("overrun_err") then
        -- Config
        i_en           <= '1';
        i_cfg_charsize     <= CFG_CHARSIZE_8BIT;
        i_cfg_stop_bits    <= CFG_STOP_BITS_1;
        baudrate       := 115200;
        i_cfg_baudrate <= std_logic_vector(to_unsigned(FCLK / (OVERSAMPLE * baudrate) - 1, i_cfg_baudrate'length));
        set_baud_rate(net, master_uart, baudrate);
        set_baud_rate(net, slave_uart, baudrate);

        wait until rising_edge(i_clk);
        i_rst <= '0';
        wait until rising_edge(i_clk);

        char := x"AA";
        push_stream(net, master_stream, char);

        wait until o_rx_data_rdy = '1';
        check(o_err_overrun = '0', "Overrun /= '0'");

        -- Should be lost data
        char := x"BB";
        push_stream(net, master_stream, char);

        char := x"CC";
        push_stream(net, master_stream, char);

        wait for 1 sec * (real(10)/real(baudrate));
        check(o_err_overrun = '1', "Overrun /= '1'");
        wait for 1 sec * (real(10)/real(baudrate));

        i_rx_re <= '1';
        wait until rising_edge(i_clk);

        i_tx_we <= '1';
        i_tx_data    <= o_rx_data;
        wait until rising_edge(i_clk);

        exp_char := x"AA";
        check_equal(i_tx_data, exp_char);
        i_tx_data    <= o_rx_data;
        wait until rising_edge(i_clk);

        -- x"CC" will not be pushed in the regfile
        exp_char := x"BB";
        check_equal(i_tx_data, exp_char);

        i_tx_we <= '0';
        i_rx_re <= '0';
        wait until rising_edge(i_clk);

        check(o_rx_data_rdy = '0', "All data should have been read");

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
    i_clk               => i_clk,
    i_rst               => i_rst,
    i_rx_re             => i_rx_re,
    i_tx_we             => i_tx_we,
    o_rx_data_rdy       => o_rx_data_rdy,
    i_tx_data           => i_tx_data,
    o_rx_data           => o_rx_data,
    o_err_parity        => o_err_parity,
    o_err_framing       => o_err_framing,
    o_err_overrun       => o_err_overrun,
    o_err_underrun      => o_err_underrun,
    o_tx_data_rdy       => o_tx_data_rdy,
    i_en                => i_en,
    i_cfg_charsize      => i_cfg_charsize,
    i_cfg_autobaud_en   => i_cfg_autobaud_en,
    i_cfg_autobaud_mode => i_cfg_autobaud_mode,
    i_cfg_parity_en     => i_cfg_parity_en,
    i_cfg_parity_type   => i_cfg_parity_type,
    i_cfg_stop_bits     => i_cfg_stop_bits,
    i_cfg_baudrate      => i_cfg_baudrate,
    o_tx                => o_tx,
    i_rx                => i_rx
  );


  uart_master_inst : entity vunit_lib.uart_master
    generic map(
      uart => master_uart
    )
    port map(
      tx => i_rx
    );


  uart_slave_inst : entity vunit_lib.uart_slave
    generic map(
      uart => slave_uart
    )
    port map(
      rx => o_tx
    );

end architecture;

