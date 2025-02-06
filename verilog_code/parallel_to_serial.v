`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 2025/01/19 19:20:06
// Design Name: 
// Module Name: parallel_to_serial
// Project Name: 
// Target Devices: 
// Tool Versions: 
// Description: 
// 
// Dependencies: 
// 
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
// 
//////////////////////////////////////////////////////////////////////////////////


module parallel_to_serial(
    input clk,
    input reset_n,
    input [17:0] data1,
    input [17:0] data2,
    input valid_in,
    output reg [17:0] data,
    output reg [3:0] addr,
    output reg wea 

    );

    reg [3:0] addr_next;
    reg [17:0] data_next;
    reg [17:0] data2_reg;
    reg wea_next;
    reg parallel_counter, parallel_counter_next;

    always @(posedge clk or negedge reset_n) begin
      if(! reset_n) begin
        addr <= 0;
        data <= 0;
        wea <= 0;
        parallel_counter <= 0;
      end else begin
        addr <= addr_next;
        data <= data_next;
        wea <= wea_next;
        parallel_counter <= parallel_counter_next;
      end
    
    end

    always @(*) begin
        addr_next = addr;
        data_next = data;
        wea_next = 1'b0;
        parallel_counter_next = parallel_counter;
        if(valid_in | parallel_counter == 1'b1) begin
            wea_next = 1'b1;
            parallel_counter_next = parallel_counter + 1;
            addr_next = addr_next + 1;
            if(valid_in) begin
                data2_reg = data2;
                data_next = data1;
            end else begin
                data_next = data2_reg;
            end
        end


    end

endmodule
