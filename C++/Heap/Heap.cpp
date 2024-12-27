#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

/**
 * Classe que implementa um Heap (MaxHeap ou MinHeap) com funcionalidades
 * como construção, inserção, remoção, alteração de prioridade e ordenação.
 */
class Heap {

private:
    bool isMaxHeap;                       //<- Define se o Heap será um MaxHeap (true) ou MinHeap (false).

    /**
     * Compara dois valores com base no tipo de Heap.
     *
     * @param parent O valor do nó pai.
     * @param child O valor do nó filho.
     * @return true se a comparação estiver de acordo com o tipo de Heap.
     */
    bool compare(const int parent, const int child) const {
        return isMaxHeap ? parent < child : parent > child;
    }

    /**
     * Realiza o processo de heapify para cima, garantindo a propriedade do Heap.
     *
     * @param heap O vetor que representa o Heap.
     * @param index O índice do nó atual.
     */
    void heapifyUp(vector<int>& heap, const int index) {
        if (index > 1) {
            if (const int parent = index / 2; compare(heap[parent], heap[index])) {
                swap(heap[parent], heap[index]);
                heapifyUp(heap, parent);
            }
        }
    }

    /**
     * Realiza o processo de heapify para baixo, garantindo a propriedade do Heap.
     *
     * @param heap O vetor que representa o Heap.
     * @param index O índice do nó atual.
     * @param size Tamanho efetivo do Heap (opcional).
     */
    void heapifyDown(vector<int>& heap, const int index, const int size = -1) {
        const int effectiveSize = (size == -1) ? heap.size() - 1 : size;
        const int left = 2 * index;
        const int right = left + 1;
        int largest = index;

        if (left <= effectiveSize && compare(heap[largest], heap[left])) {
            largest = left;
        }

        if (right <= effectiveSize && compare(heap[largest], heap[right])) {
            largest = right;
        }

        if (largest != index) {
            swap(heap[index], heap[largest]);
            heapifyDown(heap, largest, effectiveSize);
        }
    }

public:
    /**
     * Construtor da classe Heap.
     *
     * @param maxHeap Define se será um MaxHeap (true) ou MinHeap (false).
     */
    explicit Heap(const bool maxHeap = true) : isMaxHeap(maxHeap) {}

    /**
     * Constrói um Heap a partir de um vetor de entrada.
     *
     * @param heap O vetor que representará o Heap.
     * @param arr O vetor de entrada.
     */
    void makeHeap(vector<int>& heap, const vector<int>& arr) {
        heap = {0};
        heap.insert(heap.end(), arr.begin(), arr.end());

        for (int i = (heap.size() - 1) / 2; i >= 1; --i) {
            heapifyDown(heap, i);
        }
    }

    /**
     * Insere um valor no Heap e ajusta a estrutura.
     *
     * @param heap O vetor que representa o Heap.
     * @param value O valor a ser inserido.
     */
    void insert(vector<int>& heap, const int value) {
        heap.push_back(value);
        heapifyUp(heap, heap.size() - 1);
    }

    /**
     * Remove o elemento da raiz do Heap.
     *
     * @param heap O vetor que representa o Heap.
     * @return O valor removido.
     * @throws runtime_error Se o Heap estiver vazio.
     */
    int remove(vector<int>& heap) {
        if (heap.size() <= 1) {
            throw runtime_error("Heap está vazio!");
        }

        const int root = heap[1];
        heap[1] = heap.back();
        heap.pop_back();
        heapifyDown(heap, 1);
        return root;
    }

    /**
     * Altera a prioridade de um elemento no Heap.
     *
     * @param heap O vetor que representa o Heap.
     * @param index O índice do elemento a ser alterado.
     * @param newValue O novo valor do elemento.
     * @throws out_of_range Se o índice estiver fora dos limites.
     */
    void changePriority(vector<int>& heap, const int index, const int newValue) {
        if (index < 1 || index >= heap.size()) {
            throw out_of_range("Índice fora do intervalo!");
        }

        const int oldValue = heap[index];
        heap[index] = newValue;

        if (compare(oldValue, newValue)) {
            heapifyUp(heap, index);
        } else {
            heapifyDown(heap, index);
        }
    }

    /**
     * Exibe os elementos do Heap.
     *
     * @param heap O vetor que representa o Heap.
     */
    static void showHeap(const vector<int>& heap) {
        for (size_t i = 1; i < heap.size(); ++i) {
            cout << heap[i] << " ";
        }
        cout << endl;
    }

    /**
     * Realiza a ordenação de um vetor utilizando o HeapSort.
     *
     * @param arr O vetor a ser ordenado.
     */
    void heapSort(vector<int>& arr) {
        vector<int> heap;
        makeHeap(heap, arr);

        for (int i = arr.size() - 1; i >= 0; --i) {
            arr[i] = heap[1];
            heap[1] = heap[heap.size() - 1];
            heap.pop_back();
            heapifyDown(heap, 1, heap.size() - 1);
        }
    }
};
