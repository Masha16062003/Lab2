class SumThread extends Thread {
    private int id;
    private int step;
    private int sum = 0;
    private int count = 0;

    public SumThread(int id, int step) {
        this.id = id;
        this.step = step;
    }

    @Override
    public void run() {
        for (int i = 0; ; i += step) {
            sum += i;
            count++;
            try {
                Thread.sleep(100);
            } catch (InterruptedException e) {
                break;
            }
        }
        System.out.println("Thread " + id + ": sum = " + sum + ", count = " + count);
    }
}

public class Main {
    public static void main(String[] args) throws InterruptedException {
        int numThreads = 3;
        SumThread[] threads = new SumThread[numThreads];
        for (int i = 0; i < numThreads; i++) {
            threads[i] = new SumThread(i, i + 1);
            threads[i].start();
        }
        Thread.sleep(5000);
        for (SumThread thread : threads) {
            thread.interrupt();
        }
        for (SumThread thread : threads) {
            thread.join();
        }
    }
}
