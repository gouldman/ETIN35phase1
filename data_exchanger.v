`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 2025/01/17 21:04:18
// Design Name: 
// Module Name: data_exchanger
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


module data_exchanger(
    input clk,
    input reset_n,
    input element_valid,
    input [7:0] x_element_in,
    input [4:0] address,
    output reg [7:0] x_element_out,
    output reg is_ready
    );
    reg [4:0] address_reg;
    reg [7:0] x_element_in_reg;
    reg element_valid_reg;
    reg is_ready_next;           
    reg [4:0] element_counter, element_counter_next;
    reg [7:0] x_elements [31:0];
    reg [7:0] x_element_out_next;
    
    

    always @(posedge clk or negedge reset_n) begin
        if(!reset_n) begin
            is_ready <= 0;
            element_counter <= 0;
            x_element_out<= 0;
            element_valid_reg <= 0;
            x_element_in_reg <= 0;
            address_reg <= 0;
        end else begin  
            is_ready <= is_ready_next;
            element_counter <= element_counter_next;
            x_element_out <= x_element_out_next;
            element_valid_reg <= element_valid;
            x_element_in_reg <= x_element_in;
            address_reg <= address;
        end
    end                 
    
    always @(*) begin
        element_counter_next = element_counter;
        is_ready_next = is_ready;
        x_element_out_next = x_element_out;
        if(element_valid_reg) begin
            x_elements[element_counter] = x_element_in_reg;
            if(element_counter == 31) begin
                is_ready_next = 1'b1;
            end else begin
                element_counter_next = element_counter + 1;            
            end
        end
        if(is_ready) begin                                   
            x_element_out_next = x_elements[address_reg];
        end
    end
endmodule
