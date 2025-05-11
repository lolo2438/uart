# PRELIMINARY DATASHEET

## INTRODUCTION


## FEATURES

16x oversampling

Charsize selection
    - 5 to 8 bits
    - Parity/address mode

interrupt lines
    - TX buffer empty
    - RX data ready

Typical errors
   - parity :
   - framing
   - overrun
   - underrun

TODO:
Autobaud detection
RX and TX fifo

## INPUTS

### CONTROL
i_uart_clk      : Clock of the UART module
i_uart_srst_n   : Synchronous reset low
i_uart_arst_n   : Asynchronous reset low

### DATA
i_uart_rx       : PHY receive pin
i_data_tx       : Data vector to transmit

### CONFIGURATION
i_uart_en       : '1' Enable the uart, '0' Disable the uart
i_charsize      : "00": "01": "10": "11":
i_framecfg      : "00": standard, "01": Parity enable "10": Address mode, Data "11": Address mode, address
i_parity_type   : '0': odd, '1': even
i_stop_bits     : '0': 1 stop bit, '1': 2 stop bits

i_autobaud_en   : '0': no autobaud, '1': autobaud mode, synchronization pattern set by autobaud_cfg
i_autobaud_cfg  : "00", "01", "10", "11"

// add fifo here

## OUTPUTS



