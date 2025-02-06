`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 2025/01/18 07:54:21
// Design Name: 
// Module Name: test
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


module test(
    input clk,
    input reset_n,
    input start
    );
    reg start_reg;
    reg [3:0] counter, counter_next;
    always@(posedge clk or negedge reset_n) begin
    if(!reset_n) begin
        counter <= 0;
        start_reg <= 0;
        end else begin
            counter <= counter_next;
            start_reg <= start;
        end
    end
    
    always @ (*) begin
        counter_next = counter;
        if(start_reg) 
        counter_next = counter + 1;
    end
endmodule
