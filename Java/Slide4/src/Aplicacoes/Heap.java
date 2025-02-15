package Aplicacoes;

import java.util.ArrayList;
import Entidades.Arvore;
import Entidades.No;

public class Heap<T> {
    private Arvore<T> arvore;
    private ArrayList<No<T>> heap;

    public Heap() {
        this.arvore = new Arvore<>();
        this.heap = new ArrayList<>();
    }

    // Constrói uma heap a partir de um vetor qualquer de IDs (para max-heap)
    // Aqui, assumiremos que os dados T são nulos ou podem ser passados depois,
    // o foco está nos IDs que definem a prioridade.
    public void construirHeap(int[] vetor) {
        // Limpar heap atual
        heap.clear();
        // Inserir todos os elementos do vetor na heap
        for (int v : vetor) {
            heap.add(new No<T>(v, null));
        }

        // Aplicar heapifyDown a partir do último nó pai até a raiz
        for (int i = (heap.size()/2) - 1; i >= 0; i--) {
            heapifyDown(i, heap.size());
        }
        reconstruirArvore();
    }

    // Heapsort utilizando max-heap para ordenar em ordem crescente
    public void heapSort(int[] vetor) {
        // Primeiro, construir a heap a partir do vetor
        construirHeap(vetor);

        // Agora, extrair o máximo repetidas vezes e colocar no final do vetor
        for (int i = vetor.length - 1; i > 0; i--) {
            // Swap do primeiro com o i-ésimo
            swap(0, i);
            // Reorganizar a heap no subvetor [0..i-1]
            heapifyDown(0, i);
        }

        // Após o loop, o vetor (refletido em heap) estará ordenado em ordem crescente.
        // Copiar de volta os IDs para o vetor original
        for (int i = 0; i < vetor.length; i++) {
            vetor[i] = heap.get(i).getId();
        }
    }

    private void swap(int i, int j) {
        No<T> temp = heap.get(i);
        heap.set(i, heap.get(j));
        heap.set(j, temp);
    }

    // heapifyDown adaptado para max-heap
    // O parâmetro 'tamanho' indica até onde considerar a heap (útil para o heapSort)
    private void heapifyDown(int index, int tamanho) {
        while (true) {
            int esq = 2 * index + 1;
            int dir = 2 * index + 2;
            int maior = index;

            if (esq < tamanho && heap.get(esq).getId() > heap.get(maior).getId()) {
                maior = esq;
            }

            if (dir < tamanho && heap.get(dir).getId() > heap.get(maior).getId()) {
                maior = dir;
            }

            if (maior != index) {
                swap(index, maior);
                index = maior;
            } else {
                break;
            }
        }
    }

    // Reconstruir a árvore a partir do heap array
    private void reconstruirArvore() {
        if (heap.isEmpty()) {
            arvore.setRaiz(null);
            return;
        }
        arvore.setRaiz(reconstruirNo(0));
    }

    private No<T> reconstruirNo(int i) {
        if (i >= heap.size()) return null;
        No<T> no = heap.get(i);
        int esqIndex = 2*i + 1;
        int dirIndex = 2*i + 2;
        no.setEsq(reconstruirNo(esqIndex));
        no.setDir(reconstruirNo(dirIndex));
        return no;
    }

    public Arvore<T> getArvore() {
        return arvore;
    }

    // Apenas para visualização ou testes
    public void imprimirHeap() {
        for (No<T> no : heap) {
            System.out.print(no.getId() + " ");
        }
        System.out.println();
    }
}

