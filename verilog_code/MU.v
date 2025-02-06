`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 2025/01/29 20:40:55
// Design Name: 
// Module Name: mu
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


module MU(
input wire clk,
input wire reset_n,
input wire [7:0] element1,
input wire [7:0] element2,
input wire [7:0] element3,
input wire [7:0] element4,
input wire [6:0]matrix_coe_1,
input wire [6:0]matrix_coe_2,
input wire input_ready,
input wire coe_ready,

output reg [17:0]result_1,
output reg [17:0]result_2,
output reg [17:0]result_3,
output reg [17:0]result_4,
output reg arthmetic_finish
    );
    
reg [17:0]result_1_next=18'b0;
reg [17:0]result_2_next=18'b0;
reg [17:0]result_3_next=18'b0;
reg [17:0]result_4_next=18'b0;
reg arthmetic_finish_next=1'b0;
reg counter=1'b0; //counter that decides which coe should be used
reg counter_next=1'b0;
reg [2:0]counter_finish=3'b0;//counter that decides when the arithmetic is finished
reg [2:0]counter_finish_next=3'b0;
reg reset_result=1'b0;
reg reset_result_next=1'b0;

always@(posedge clk or negedge reset_n) begin //sequential logic
    if(!reset_n) begin
        result_1 <= 0;
        result_2 <= 0;
        result_3 <= 0;
        result_4 <= 0;
        arthmetic_finish <= 0;
        counter <=0;
        counter_finish <=0;
        reset_result<=0;
    end 
    else begin 
       result_1 <= result_1_next;
       result_2 <= result_2_next;
       result_3 <= result_3_next;
       result_4 <= result_4_next;
       arthmetic_finish <= arthmetic_finish_next;
       counter <= counter_next;
       counter_finish <= counter_finish_next;
       if(reset_result)begin
       reset_result<=0;
       end else begin
       reset_result<=reset_result_next;
       end
    end
end

always@(*) begin  //combinational logic for multiplxier
    result_1_next=result_1;
    result_2_next=result_2;
    result_3_next=result_3;
    result_4_next=result_4;
    counter_next=counter;
    counter_finish_next=counter_finish;
    arthmetic_finish_next=0;
    reset_result_next=0;
    if(counter_finish==7) begin
        arthmetic_finish_next=1;
        counter_finish_next=0;
        reset_result_next=1;
        counter_next=counter +1;
    end 
    if(reset_result==1)begin
        counter_next=counter +1;
        counter_finish_next=counter_finish+1;
        result_1_next=element1*matrix_coe_1;
        result_2_next=element2*matrix_coe_1;
        result_3_next=element3*matrix_coe_1;
        result_4_next=element4*matrix_coe_1;   
    end
    else if(input_ready==1&coe_ready==1)begin
             counter_next=counter +1;
             counter_finish_next=counter_finish+1;
             
        if(counter==0) begin
            result_1_next=result_1+element1*matrix_coe_1;
            result_2_next=result_2+element2*matrix_coe_1;
            result_3_next=result_3+element3*matrix_coe_1;
            result_4_next=result_4+element4*matrix_coe_1;
        end
        else if(counter==1) begin
            result_1_next=result_1+element1*matrix_coe_2;
            result_2_next=result_2+element2*matrix_coe_2;
            result_3_next=result_3+element3*matrix_coe_2;
            result_4_next=result_4+element4*matrix_coe_2;
    end
    end
end


endmodule
