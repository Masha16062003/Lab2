using System;
using System.Threading;

class SumThread {
    private int id;
    private int step;
    private int sum = 0;
    private int count = 0;
    private bool shouldStop = false;

    public SumThread(int id, int step) {
        this.id = id;
        this.step = step;
    }

    public void Run() {
        try {
            for (int i = 0; !shouldStop; i += step) {
                sum += i;
                count++;
                Thread.Sleep(100);
            }
        } catch (ThreadInterruptedException) {
            // Потік перерваний
        }
    }

    public void PrintResults() {
        Console.WriteLine("Thread " + id + ": sum = " + sum + ", count = " + count);
    }

    public void RequestStop() {
        shouldStop = true;
    }
}

class Program {
    static void Main(string[] args) {
        int numThreads = 3;
        SumThread[] sumThreads = new SumThread[numThreads];
        Thread[] threads = new Thread[numThreads];

        for (int i = 0; i < numThreads; i++) {
            sumThreads[i] = new SumThread(i, i + 1);
            threads[i] = new Thread(sumThreads[i].Run);
            threads[i].Start();
        }

        Thread.Sleep(5000);

        for (int i = 0; i < numThreads; i++) {
            sumThreads[i].RequestStop();
            threads[i].Interrupt(); // Примусове переривання потоку
        }

        for (int i = 0; i < numThreads; i++) {
            threads[i].Join();
            sumThreads[i].PrintResults();
        }
    }
}
