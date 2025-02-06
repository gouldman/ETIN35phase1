`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 2025/01/27 13:50:02
// Design Name: 
// Module Name: Controller
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


module Controller (
    input wire clk,              // 时钟信号
    input wire reset_n,          // 复位信号（低电平有效）
    input wire start,            // 启动信号
    input wire arithmetic_finish,// MU 计算完成信号
    input wire [17:0] result1,   // MU1 计算结果
    input wire [17:0] result2,   // MU2 计算结果
    input wire [17:0] result3,   // MU3 计算结果
    input wire [17:0] result4,   // MU4 计算结果

    output reg input_start,      // 控制输入数据加载
    output reg [3:0] rom_addr,   // 读取 ROM 的地址
    output reg rom_start,        // 触发 ROM 读取
    output reg [17:0] result,    // 计算结果输出到 RAM
    output reg [3:0] ram_addr,   // RAM 地址
    output reg web,              // RAM 写使能
    output reg finish            // 计算完成信号
);

   // **定义状态**
    typedef enum logic [1:0] {
        IDLE = 2'b00,            // 空闲状态
        LOAD_DATA = 2'b01,       // 读取 ROM & INPUT 数据
        WRITE_RAM = 2'b10,       // 写入 RAM
        //NEXT_COL = 2'b011,
        FINISH = 2'b11           // 计算完成
    } state_t;

    state_t current_state, next_state;
    reg [3:0] col_index;          // **ROM 读取当前列索引**（范围 0-15）
    reg [4:0] load_cycle_count;   // **ROM 读取周期计数**（范围 0-1）
    reg [1:0]result_count;//结果计数器1-4
    reg [2:0]ari_count;
   // **拼接寄存器**
    reg [287:0] concatenated_result;  // 拼接后的 72-bit 结果,col1
//    reg [71:0] concatenated_result2;  // 拼接后的 72-bit 结果,col2
//    reg [71:0] concatenated_result3;  // 拼接后的 72-bit 结果,col3
//    reg [71:0] concatenated_result4;  // 拼接后的 72-bit 结果,col4
    
    reg [17:0] result_part;          // 每次写入 RAM 的 18-bit 数据
   
    
    reg [3:0] i;  
        
    // **状态转换**
    always @(posedge clk or negedge reset_n) begin
        if (!reset_n)
            current_state <= IDLE;
        else
            current_state <= next_state;
    end

    // **状态机控制逻辑**
    always @(*) begin
        // **默认信号值**
        
        next_state = current_state;

        case (current_state)
            // **空闲状态**
            IDLE: begin
             rom_start = 0;
             input_start = 0;
             web = 0;
             finish = 0;
             result = 0;
             //finish_flag =0;
             //rom_addr <= col_index;
                if (start) begin
                    next_state = LOAD_DATA;
                end
            end

            // **同时加载 INPUT 和 ROM 数据**
            LOAD_DATA: begin
                input_start = 1;  // **同步让 INPUT 右移**
                rom_start = 1;    // **第 1 个周期拉高 ROM 读取信号**

                rom_addr = col_index; // **ROM 地址按照 0-15 递增**
                
                if (ari_count == 3'd4) begin
                    next_state = WRITE_RAM; // **读取 ROM 结束，进入计算状态**
                    
                end else begin
                    next_state = LOAD_DATA; // **继续等待 ROM 数据**
                end
            end

           

            // **写入 RAM**
            WRITE_RAM: begin
                input_start = 0;  
                rom_start = 0;
                
                web = 1; // RAM 写使能
                // 从拼接的 288-bit 结果中提取 18-bit 数据写入 RAM
                result_part = concatenated_result[287:270];  // 提取最高 18 位
                result = result_part;  // 写入 result 输出到 RAM
                
                // 更新拼接结果，准备写入下一部分
                //concatenated_result = concatenated_result << 18;  // 左移 18 位，处理下一部分

                if (i < 4'd15) begin  
                    next_state = WRITE_RAM;    
                end else begin      
                    next_state = FINISH; // 继续写入下一部分数据
                    //ram_addr=ram_addr;
                end
            end
//            // **判断结果矩阵是否写完**
//            NEXT_COL: begin
//                web = 0;
//                result =0;
                 
//                if (finish_flag) begin  // **下一周期才进入 FINISH**
//                     next_state = FINISH;
//                end else begin
//                     next_state = WRITE_RAM;
//                end
//            end
            FINISH: begin
                web = 0;
                result =0;
                finish = 1;
                next_state = IDLE;
            end

            default: next_state = IDLE;
        endcase
    end

   // **计数器控制**
always @(posedge clk or negedge reset_n) begin
    if (!reset_n) begin
        col_index <= 0;
        load_cycle_count <= 0;  // 复位时确保为 0
        
        i <= 0;
        
       // concatenated_result <= 0;
        result_part <= 0;
        ram_addr <= 0;  // **确保复位时 RAM 地址归零**
        rom_addr <= 0;
        //finish_flag <= 0; 
    end 
   
    else if (current_state == LOAD_DATA) begin
        if (load_cycle_count < 5'd31) begin
            load_cycle_count <= load_cycle_count + 1;  
        end else begin
            load_cycle_count <= 0;  // 归零
        end
        
        if (load_cycle_count[0] == 1'b1) begin
            if (col_index < 4'd15) begin
                col_index <= col_index + 1;
            end else begin
                col_index <= 0; // 防止越界
            end
        end
        
    end 
    else if (current_state == WRITE_RAM) begin
       load_cycle_count <= 0;
        if (i < 4'd15) begin
            i <= i + 1;           // 计数 0 → 15
            ram_addr <= ram_addr+1;
            // 更新拼接结果，准备写入下一部分
            concatenated_result <= concatenated_result << 18;  // 左移 18 位，处理下一部分
            
        end else begin
            i <= 0;               // **所有数据写完后归零**
            ram_addr <= 0;
        end
//        if (ram_addr == 4'd15) begin  
//            finish_flag <= 1;          
//        end 
    end 
   
end

always @(posedge clk or negedge reset_n) begin
    if (!reset_n) begin
        concatenated_result <= 0;
        result_count <= 0;
        ari_count <= 0;
    end 
    else if (arithmetic_finish) begin
        // **按 result_count 选择拼接位置**
        case (result_count)
            2'd0: concatenated_result[287:216] <= {result1, result2, result3, result4}; // 第一组数据
            2'd1: concatenated_result[215:144] <= {result1, result2, result3, result4}; // 第二组数据
            2'd2: concatenated_result[143:72]  <= {result1, result2, result3, result4}; // 第三组数据
            2'd3: concatenated_result[71:0]    <= {result1, result2, result3, result4}; // 第四组数据
        endcase
       if (result_count == 3'd4) begin
            ari_count <= 0;  // **拼接完成后清零**
        end else begin
            ari_count <= result_count + 1;
        end
        // **更新 result_count，确保 4 组拼接后归零**
        if (result_count == 2'd3) begin
            result_count <= 0;  // **拼接完成后清零**
        end else begin
            result_count <= result_count + 1;
        end
    end
    else if(!arithmetic_finish)begin
        ari_count <= 0;
    end
    
end


endmodule 