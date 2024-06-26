import java.util.Arrays;

public class MinElementFinder {
    private static final int SIZE = 1000;
    private static final int THREAD_COUNT = 4;
    private static int[] array = new int[SIZE];
    private static int globalMin = Integer.MAX_VALUE;

    public static void main(String[] args) {
        generateArray();
        array[SIZE / 2] = -1;

        Thread[] threads = new Thread[THREAD_COUNT];
        for (int i = 0; i < THREAD_COUNT; i++) {
            final int start = i * (SIZE / THREAD_COUNT);
            final int end = (i + 1) * (SIZE / THREAD_COUNT);
            threads[i] = new Thread(() -> findMin(start, end));
            threads[i].start();
        }

        for (Thread thread : threads) {
            try {
                thread.join();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        System.out.println("Minimum element: " + globalMin);
    }

    private static void generateArray() {
        for (int i = 0; i < SIZE; i++) {
            array[i] = (int) (Math.random() * 100);
        }
    }

    private static synchronized void findMin(int start, int end) {
        int localMin = Integer.MAX_VALUE;
        for (int i = start; i < end; i++) {
            if (array[i] < localMin) {
                localMin = array[i];
            }
        }
        if (localMin < globalMin) {
            globalMin = localMin;
        }
    }
}
