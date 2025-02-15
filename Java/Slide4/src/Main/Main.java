package Main;

import java.util.Scanner;

import Aplicacoes.Heap;

public class Main {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);

        Heap<String> heap = new Heap<>();

        System.out.print("Digite o tamanho do vetor: ");
        int n = sc.nextInt();
        int[] vetor = new int[n];

        System.out.println("Digite os elementos do vetor:");
        for (int i = 0; i < n; i++) {
            vetor[i] = sc.nextInt();
        }

        heap.construirHeap(vetor);

        System.out.println("Heap construída (max-heap), exibindo array da heap:");
        heap.imprimirHeap();

        heap.heapSort(vetor);

        System.out.println("Vetor após heapsort (ordem crescente):");
        for (int i : vetor) {
            System.out.print(i + " ");
        }
        System.out.println();

        sc.close();
    }
}
