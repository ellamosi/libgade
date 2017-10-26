with Test_LCD_Output; use Test_LCD_Output;

procedure TC_CPU_Instrs_op_a_hl is
begin
   Run_LCD_Test ("11-op a,(hl).gb", "CPU Instructions: op a,(hl)", 1100);
end TC_CPU_Instrs_op_a_hl;
