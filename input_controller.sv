`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 2025/02/01 21:40:46
// Design Name: 
// Module Name: input_controller
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

module input_controller(
input wire clk,
input wire reset_n,
input wire [7:0] data,
input wire input_start,
input wire data_valid,

output reg start,
output reg [7:0] element1,
output reg [7:0] element2,
output reg [7:0] element3,
output reg [7:0] element4,
output reg input_ready
    );

reg [7:0] element1_next;
reg [7:0] element2_next;
reg [7:0] element3_next;
reg [7:0] element4_next;

reg [63:0] shift_1=64'b0;
reg [63:0] shift_1_next=64'b0;
reg [63:0] shift_2=64'b0;
reg [63:0] shift_2_next=64'b0;
reg [63:0] shift_3=64'b0;
reg [63:0] shift_3_next=64'b0;
reg [63:0] shift_4=64'b0;
reg [63:0] shift_4_next=64'b0;

reg [4:0] count=5'b0;
reg [4:0] count_next=5'b0;
reg [1:0] shift_count=2'b0;
reg [1:0] shift_count_next=2'b0;
reg start_next=1'b0;
reg input_ready_next=1'b0;



always@(posedge clk or negedge reset_n)begin
    if(!reset_n) begin
        shift_1<=0;
        shift_2<=0;
        shift_3<=0;
        shift_4<=0;

        input_ready <=0;
        count<=0;
        shift_count <= 0;
        start<=0;//**增加几个初始化，避免仿真波形出现红色的X不确定状态**
        
        element1<=0;
        element2<=0;
        element3<=0;
        element4<=0;
    end
    else begin
        shift_1<=shift_1_next;
        shift_2<=shift_2_next;
        shift_3<=shift_3_next;
        shift_4<=shift_4_next;

        input_ready<=input_ready_next;
        count<=count_next;
        shift_count<=shift_count_next;
        start<=start_next;

        element1<=element1_next;
        element2<=element2_next;
        element3<=element3_next;
        element4<=element4_next;
    end
end

always@(*) begin
    start_next = 0;
    count_next = count;
    shift_count_next = shift_count;
    shift_1_next = shift_1;
    shift_2_next = shift_2;
    shift_3_next = shift_3;
    shift_4_next = shift_4;   
    element1_next = element1;
    element2_next = element2;
    element3_next = element3;
    element4_next = element4;
    input_ready_next = 0;
    if(data_valid) begin
        count_next = count + 1;
        shift_count_next = shift_count + 1;
        if(count == 31) begin
            start_next = 1'b1;
        end
        if(shift_count == 2'b00) begin
            shift_1_next = (shift_1 << 8) | data;

        end else
        if(shift_count == 2'b01) begin
            shift_2_next = (shift_2 << 8) | data;
        end  else
        if(shift_count == 2'b10) begin
            shift_3_next = (shift_3 << 8) | data;
        end  else 
        begin
            shift_4_next = (shift_4 << 8) | data;
        end
    end else if(input_start==1) begin
        input_ready_next=1;
        element1_next=shift_1[63:56];   //get the right 8-bit output
        element2_next=shift_2[63:56];
        element3_next=shift_3[63:56];
        element4_next=shift_4[63:56];
        
        shift_1_next=shift_1<<8 | shift_1[63:56]; //shift64-bit reg
        shift_2_next=shift_2<<8 | shift_2[63:56];
        shift_3_next=shift_3<<8 | shift_3[63:56];
        shift_4_next=shift_4<<8 | shift_4[63:56];
     end
end



endmodule
// `timescale 1ns / 1ps
// //////////////////////////////////////////////////////////////////////////////////
// // Company: 
// // Engineer: 
// // 
// // Create Date: 2025/02/01 21:40:46
// // Design Name: 
// // Module Name: input_controller
// // Project Name: 
// // Target Devices: 
// // Tool Versions: 
// // Description: 
// // 
// // Dependencies: 
// // 
// // Revision:
// // Revision 0.01 - File Created
// // Additional Comments:
// // 
// //////////////////////////////////////////////////////////////////////////////////

// module input_controller(
// input wire clk,
// input wire reset_n,
// input wire [7:0] data,
// input wire input_start,
// input wire data_valid,

// output reg start,
// output reg [7:0] element1,
// output reg [7:0] element2,
// output reg [7:0] element3,
// output reg [7:0] element4,
// output reg input_ready
//     );

// reg [7:0] element1_next;
// reg [7:0] element2_next;
// reg [7:0] element3_next;
// reg [7:0] element4_next;

// reg [63:0] shift_1=64'b0;
// reg [63:0] shift_1_next=64'b0;
// reg [63:0] shift_2=64'b0;
// reg [63:0] shift_2_next=64'b0;
// reg [63:0] shift_3=64'b0;
// reg [63:0] shift_3_next=64'b0;
// reg [63:0] shift_4=64'b0;
// reg [63:0] shift_4_next=64'b0;

// reg [4:0] count=5'b0;
// reg [4:0] count_next=5'b0;
// reg [1:0] shift_count=2'b0;
// reg [1:0] shift_count_next=2'b0;
// reg start_next=1'b0;
// reg input_ready_next=1'b0;



// always@(posedge clk or negedge reset_n)begin
//     if(!reset_n) begin
//         shift_1<=0;
//         shift_2<=0;
//         shift_3<=0;
//         shift_4<=0;

//         input_ready <=0;
//         count<=0;
//         shift_count <= 0;
//         start<=0;//**增加几个初始化，避免仿真波形出现红色的X不确定状态**
        
//         element1<=0;
//         element2<=0;
//         element3<=0;
//         element4<=0;
//     end
//     else begin
//         shift_1<=shift_1_next;
//         shift_2<=shift_2_next;
//         shift_3<=shift_3_next;
//         shift_4<=shift_4_next;

//         input_ready<=input_ready_next;
//         count<=count_next;
//         shift_count<=shift_count_next;
//         start<=start_next;

//         element1<=element1_next;
//         element2<=element2_next;
//         element3<=element3_next;
//         element4<=element4_next;
//     end
// end

// always @(*) begin
//     // 默认值
//     start_next = 0;
//     count_next = count;
//     shift_count_next = shift_count;
//     shift_1_next = shift_1;
//     shift_2_next = shift_2;
//     shift_3_next = shift_3;
//     shift_4_next = shift_4;
//     element1_next = element1;
//     element2_next = element2;
//     element3_next = element3;
//     element4_next = element4;
//     input_ready_next = 0;

//     // 优先级：input_start > data_valid
//     if (input_start) begin
//         input_ready_next = 1;
//         element1_next = shift_1[63:56];
//         element2_next = shift_2[63:56];
//         element3_next = shift_3[63:56];
//         element4_next = shift_4[63:56];
//         // 普通左移，低位补0
//         shift_1_next = shift_1 << 8|shift_1[63:56];
//         shift_2_next = shift_2 << 8|shift_2[63:56];
//         shift_3_next = shift_3 << 8|shift_3[63:56];
//         shift_4_next = shift_4 << 8|shift_4[63:56];
//     end
//     else if (data_valid) begin
//         count_next = count + 1;
//         shift_count_next = (shift_count == 2'b11) ? 2'b0 : shift_count + 1;
//         if (count == 31) start_next = 1;
//         case (shift_count)
//             2'b00: shift_1_next = (shift_1 << 8) | data;
//             2'b01: shift_2_next = (shift_2 << 8) | data;
//             2'b10: shift_3_next = (shift_3 << 8) | data;
//             2'b11: shift_4_next = (shift_4 << 8) | data;
//         endcase
//     end
// end
 

// endmodule
