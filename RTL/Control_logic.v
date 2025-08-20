`timescale 1ns/1ps
`default_nettype none
`include "Def_name.v"

module control_logic #(
    parameter DATA_WIDTH = 8
)(
    input  wire                    PCLK,
    input  wire                    PRESET_n,

    input  wire [DATA_WIDTH-1:0]   TDR_reg,
    input  wire [DATA_WIDTH-1:0]   TCR_reg,
    input  wire                    TDR_WR_pulse,

    output wire [1:0]              Cks,
    output wire                    Load_Tdr,
    output wire [DATA_WIDTH-1:0]   count_start_value,
    output wire                    count_up_down,
    output wire                    count_enable
);

  // ===== Registers =====
  reg              load_q, load_qq;        // sync + delay cho bit LOAD
  reg              load_tdr_r;             // xung Load 1 chu kỳ
  reg [DATA_WIDTH-1:0] start_val_r;        // giá trị nạp vào TCNT

  // ===== Sequential logic (đồng bộ hoàn toàn) =====
  always @(posedge PCLK or negedge PRESET_n) begin
    if (!PRESET_n) begin
      load_q      <= 1'b0;
      load_qq     <= 1'b0;
      load_tdr_r  <= 1'b0;
      start_val_r <= {DATA_WIDTH{1'b0}};
    end else begin
      // sample bit LOAD
      load_q  <= TCR_reg[`TCR_load_bit];
      load_qq <= load_q;

      // mặc định hạ xung
      load_tdr_r <= 1'b0;

      // Điều kiện tạo LOAD (1 chu kỳ):
      // 1) Khi ghi TDR -> nạp ngay và tạo xung
      if (TDR_WR_pulse) begin
        start_val_r <= TDR_reg;
        load_tdr_r  <= 1'b1;
      end
      // 2) Khi phát hiện cạnh lên LOAD trong TCR
      else if (load_q & ~load_qq) begin
        // Lấy giá trị đang có trong TDR_reg để nạp
        start_val_r <= TDR_reg;
        load_tdr_r  <= 1'b1;
      end
      // không tự ý đổi start_val_r ở các trường hợp khác
    end
  end

  // ===== Kết nối ra ngoài =====
  assign Load_Tdr          = load_tdr_r;
  assign count_start_value = start_val_r;

  assign Cks           = TCR_reg[`TCR_Cks_1:`TCR_Cks_0];
  assign count_up_down = TCR_reg[`TCR_Up_Dw_bit];
  assign count_enable  = TCR_reg[`TCR_En_bit];

endmodule

`default_nettype wire
