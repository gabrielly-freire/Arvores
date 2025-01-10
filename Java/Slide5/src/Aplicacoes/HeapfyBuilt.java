package Aplicacoes;

public class HeapfyBuilt {

    public void buildHeap(int[] arr) {
        int n = arr.length;
        for (int i = (n / 2) - 1; i >= 0; i--) {
            heapify(arr, n, i);
        }
    }

    private void heapify(int[] arr, int size, int i) {
        int maior = i;
        int esquerda = 2 * i + 1;
        int direita = 2 * i + 2;

        if (esquerda < size && arr[esquerda] > arr[maior]) {
            maior = esquerda;
        }

        if (direita < size && arr[direita] > arr[maior]) {
            maior = direita;
        }

        if (maior != i) {
            trocar(arr, i, maior);
            heapify(arr, size, maior);
        }
    }

    private void trocar(int[] arr, int i, int j) {
        int temp = arr[i];
        arr[i] = arr[j];
        arr[j] = temp;
    }

    public void heapSort(int[] arr) {
        int n = arr.length;
        buildHeap(arr);
        for (int i = n - 1; i > 0; i--) {
            trocar(arr, 0, i);
            heapify(arr, i, 0);
        }
    }

    public void imprimeArray(int[] arr) {
        for (int valor : arr) {
            System.out.print(valor + " ");
        }
        System.out.println();
    }
}
