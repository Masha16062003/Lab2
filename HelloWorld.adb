with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Calendar; use Ada.Calendar;

procedure HelloWorld is
   task type Sum_Task is
      entry Start(Task_Id : Natural; Task_Step : Natural);
      entry Stop;
   end Sum_Task;

   task body Sum_Task is
      Sum     : Natural := 0;
      Count   : Natural := 0;
      T       : Ada.Real_Time.Time;
      Stopped : Boolean := False;
      Id      : Natural := 0;
      Step    : Natural := 0;
   begin
      accept Start(Task_Id : in Natural; Task_Step : in Natural) do
         Id := Task_Id;
         Step := Task_Step;
         Put_Line("Task " & Natural'Image(Id) & " started with step " & Natural'Image(Step));
      end Start;

      T := Clock;
      loop
         select
            accept Stop do
               Stopped := True;
               Put_Line("Task " & Natural'Image(Id) & " received stop signal");
            end Stop;
            exit;
         or
            delay until T + Milliseconds(50);
            T := T + Milliseconds(50);
         end select;

         exit when Stopped;

         Sum := Sum + Step * Count;
         Count := Count + 1;
      end loop;
      Put_Line("Task " & Natural'Image(Id) & ": Sum = " & Natural'Image(Sum) & ", Count = " & Natural'Image(Count) & ", Stopped = " & Boolean'Image(Stopped));
   end Sum_Task;

   task type Controller is
      entry Start;
   end Controller;

   task body Controller is
      type Sum_Task_Access is access all Sum_Task;
      Tasks : array (1 .. 3) of Sum_Task_Access;
   begin
      for I in 1 .. 3 loop
         Tasks(I) := new Sum_Task;
      end loop;

      accept Start;

      for I in 1 .. 3 loop
         delay 0.05;
         Put_Line("Starting task " & Integer'Image(I));
         Tasks(I).Start(I, I + 1);
      end loop;

      delay 2.0;

      for I in 1 .. 3 loop
         Put_Line("Stopping task " & Integer'Image(I));
         Tasks(I).Stop;
         delay 0.5;  -- Ensure task has time to process stop
      end loop;

      delay 1.0;  -- Additional delay to ensure all tasks stop properly

      Put_Line("All tasks should be stopped now.");
   end Controller;

begin
   declare
      C : Controller;
   begin
      C.Start;
   end;
end HelloWorld;
