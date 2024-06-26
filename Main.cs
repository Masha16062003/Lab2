using System;
using System.Threading;

class MinElementFinder
{
    private const int SIZE = 1000;
    private const int THREAD_COUNT = 4;
    private static int[] array = new int[SIZE];
    private static int globalMin = int.MaxValue;
    private static readonly object lockObj = new object();

    static void Main()
    {
        // Generate array and add a negative number
        GenerateArray();
        array[SIZE / 2] = -1; // Insert a negative number

        // Create and start threads
        Thread[] threads = new Thread[THREAD_COUNT];
        for (int i = 0; i < THREAD_COUNT; i++)
        {
            int start = i * (SIZE / THREAD_COUNT);
            int end = (i + 1) * (SIZE / THREAD_COUNT);
            threads[i] = new Thread(() => FindMin(start, end));
            threads[i].Start();
        }

        // Wait for threads to complete
        foreach (Thread thread in threads)
        {
            thread.Join();
        }

        // Print result
        Console.WriteLine("Minimum element: " + globalMin);
    }

    private static void GenerateArray()
    {
        Random rand = new Random();
        for (int i = 0; i < SIZE; i++)
        {
            array[i] = rand.Next(100);
        }
    }

    private static void FindMin(int start, int end)
    {
        int localMin = int.MaxValue;
        for (int i = start; i < end; i++)
        {
            if (array[i] < localMin)
            {
                localMin = array[i];
            }
        }
        lock (lockObj)
        {
            if (localMin < globalMin)
            {
                globalMin = localMin;
            }
        }
    }
}
