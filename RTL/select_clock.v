module select_clock (
  input  wire       PCLK,
  input  wire       PRESET_n,
  input  wire [1:0] Cks,              // 00:/2 01:/4 10:/8 11:/16
  output wire       Clock_counter     // mức clock đã chọn (chưa bắt cạnh)
);
  reg [3:0] Clk_in;
  always @(posedge PCLK or negedge PRESET_n) begin
    if (!PRESET_n) Clk_in <= 4'h0;
    else          Clk_in <= Clk_in + 4'h1;
  end

  assign Clock_counter =
      (Cks==2'b00) ? Clk_in[0] :
      (Cks==2'b01) ? Clk_in[1] :
      (Cks==2'b10) ? Clk_in[2] :
                     Clk_in[3] ;
endmodule

`default_nettype wire