`timescale 1ns/1ps
`default_nettype none

module timer_counter #(
  parameter DATA_WIDTH = 8
)(
  input  wire                    PCLK,
  input  wire                    PRESET_n,
  input  wire                    Clock_counter,       // slow clock enable or pulse
  input  wire [DATA_WIDTH-1:0]   count_start_value,
  input  wire                    count_load,
  input  wire                    count_enable,
  input  wire                    count_up_down,       // 0 = up, 1 = down
  output reg  [DATA_WIDTH-1:0]   TCNT_Out,
  output reg                     Set_OVF_pulse,       // 1-cycle pulse on overflow
  output reg                     Set_UDF_pulse        // 1-cycle pulse on underflow
);

  reg prev_clk; // to detect rising edge of Clock_counter relative to PCLK

  always @(posedge PCLK or negedge PRESET_n) begin
    if (!PRESET_n) begin
      TCNT_Out      <= {DATA_WIDTH{1'b0}};
      prev_clk      <= 1'b0;
      Set_OVF_pulse <= 1'b0;
      Set_UDF_pulse <= 1'b0;
    end else begin
      // default clear pulses each PCLK
      Set_OVF_pulse <= 1'b0;
      Set_UDF_pulse <= 1'b0;

      // sample edge on Clock_counter
      if (~prev_clk && Clock_counter) begin
        // rising edge detected - perform counting logic
        if (count_load) begin
          TCNT_Out <= count_start_value;
        end else if (count_enable) begin
          if (!count_up_down) begin
            // count up
            if (TCNT_Out == {DATA_WIDTH{1'b1}}) begin
              TCNT_Out <= {DATA_WIDTH{1'b0}}; // wrap
              Set_OVF_pulse <= 1'b1;
            end else begin
              TCNT_Out <= TCNT_Out + 1'b1;
            end
          end else begin
            // count down
            if (TCNT_Out == {DATA_WIDTH{1'b0}}) begin
              TCNT_Out <= {DATA_WIDTH{1'b1}}; // wrap
              Set_UDF_pulse <= 1'b1;
            end else begin
              TCNT_Out <= TCNT_Out - 1'b1;
            end
          end
        end
      end

      prev_clk <= Clock_counter;
    end
  end

endmodule
