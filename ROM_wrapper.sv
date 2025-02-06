`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 2025/02/03 15:25:36
// Design Name: 
// Module Name: ROM_wrapper
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


module ROM_wrapper(
    input wire clk,
    input wire reset_n,
    input wire rom_start,
    input wire [6:0] rom_addr,
    output logic [6:0] matrix_coe_1,
    output logic [6:0] matrix_coe_2,
    output reg coe_ready
    );

    wire [13:0] q;
    reg coe_ready_next;
    ROM ROM_INST (
        .clk  (clk),
        .reset_n (reset_n),
        .rom_addr   (rom_addr[4:0]),
        .q   (q)
    );
    assign matrix_coe_1 = q[6:0];
    assign matrix_coe_2 = q[13:7];
    always @(posedge clk or negedge reset_n) begin
        if(!reset_n)
            coe_ready <= 0;
        else 
            coe_ready <= coe_ready_next;
    end
    always @(*)begin
        coe_ready_next = coe_ready;
        if(rom_start)
            coe_ready_next = 1;
    end
endmodule
