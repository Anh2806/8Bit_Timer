`ifndef _DEF_NAME_V_
`define _DEF_NAME_V_

// Reset values
`define TDR_RST   {`DATA_WIDTH{1'b0}}
`define TCR_RST   {`DATA_WIDTH{1'b0}}
`define TSR_RST   {`DATA_WIDTH{1'b0}}
`define TCNT_RST  {`DATA_WIDTH{1'b0}}

// ADDRESS (APB)
`define TDR_ADDR  8'h00
`define TCR_ADDR  8'h01
`define TSR_ADDR  8'h02
`define TCNT_ADDR 8'h03

// TCR bit
`define TCR_load_bit 7      // bit load TDR to TCNT
`define TCR_Up_Dw_bit 5
`define TCR_En_bit    4
`define TCR_Cks_1     1
`define TCR_Cks_0     0

// TSR bit
`define TMR_UDF_bit   1     // Underflow flag
`define TMR_OVF_bit   0     // Overflow flag

`endif
