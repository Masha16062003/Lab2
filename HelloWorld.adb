with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;

procedure Min_Element_Finder is
   SIZE : constant := 1000;
   THREAD_COUNT : constant := 4;
   type Int_Array is array (1 .. SIZE) of Integer;
   My_Array : Int_Array;
   Global_Min : Integer := Integer'Last;

   task type Min_Finder is
      entry Set_Range(Start_Index : Integer; End_Index : Integer);
   end Min_Finder;

   protected Global_Min_Access is
      procedure Update_Global_Min(Value : Integer);
      function Get_Global_Min return Integer;
   private
      Global_Min : Integer := Integer'Last;
   end Global_Min_Access;

   protected body Global_Min_Access is
      procedure Update_Global_Min(Value : Integer) is
      begin
         if Value < Global_Min then
            Global_Min := Value;
         end if;
      end Update_Global_Min;

      function Get_Global_Min return Integer is
      begin
         return Global_Min;
      end Get_Global_Min;
   end Global_Min_Access;

   task body Min_Finder is
      Start_Idx, End_Idx : Integer;
      Local_Min : Integer := Integer'Last;
   begin
      accept Set_Range(Start_Index : Integer; End_Index : Integer) do
         Start_Idx := Start_Index;
         End_Idx := End_Index;
      end Set_Range;

      for I in Start_Idx .. End_Idx loop
         if My_Array(I) < Local_Min then
            Local_Min := My_Array(I);
         end if;
      end loop;

      -- Critical section to update global_min
      Global_Min_Access.Update_Global_Min(Local_Min);
   end Min_Finder;

   Tasks : array (1 .. THREAD_COUNT) of Min_Finder;

   procedure Generate_Array is
      package Random_Int is new Ada.Numerics.Discrete_Random(Positive);
      Gen : Random_Int.Generator;
   begin
      Random_Int.Reset(Gen);
      for I in 1 .. SIZE loop
         My_Array(I) := Random_Int.Random(Gen);
      end loop;
      My_Array(SIZE / 2) := -1;  -- Insert a negative number
   end Generate_Array;

begin
   Generate_Array;

   for I in 1 .. THREAD_COUNT loop
      Tasks(I).Set_Range((I - 1) * (SIZE / THREAD_COUNT) + 1, I * (SIZE / THREAD_COUNT));
   end loop;

   -- Wait for tasks to complete
   delay 0.5;  -- A simple delay to ensure all tasks have time to complete

   Put_Line("Minimum element: " & Integer'Image(Global_Min_Access.Get_Global_Min));
end Min_Element_Finder;
