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

	/**
	 * Função auxiliar: troca o conteúdo de dois nós no array.
	 */
	private void swap(int i, int j) {
		No<T> temp = heap.get(i);
		heap.set(i, heap.get(j));
		heap.set(j, temp);
	}

	private void reconstruirArvore() {
		if (heap.isEmpty()) {
			arvore.setRaiz(null);
			return;
		}

		// Reconstrói a partir do índice 0
		arvore.setRaiz(reconstruirNo(0));
	}

	private No<T> reconstruirNo(int i) {
		if (i < 0 || i >= heap.size())
			return null;
		No<T> no = heap.get(i);
		no.setEsq(null);
		no.setDir(null);
		int esqIndex = 2 * i + 1;
		int dirIndex = 2 * i + 2;
		if (esqIndex < heap.size()) {
			no.setEsq(reconstruirNo(esqIndex));
		}
		if (dirIndex < heap.size()) {
			no.setDir(reconstruirNo(dirIndex));
		}
		return no;
	}

	/**
	 * Insere um novo nó na heap mantendo a propriedade do heap.
	 */
	public void inserir(int id, T dados) {
		No<T> novo = new No<>(id, dados);
		heap.add(novo);
		heapifyUp(heap.size() - 1);
		reconstruirArvore();
	}

	/**
	 * Remove a raiz da heap e retorna seus dados.
	 */
	public T removerRaiz() {
		if (heap.isEmpty())
			return null;

		// Raiz
		No<T> raiz = heap.get(0);

		// Substitui a raiz pelo último nó
		No<T> ultimo = heap.remove(heap.size() - 1);
		if (!heap.isEmpty()) {
			heap.set(0, ultimo);
			heapifyDown(0);
		}

		reconstruirArvore();
		return raiz.getDados();
	}

	/**
	 * Alterar a prioridade (id) de um nó específico na heap.
	 */
	public void alterarPrioridade(No<T> noAlvo, int novaPrioridade) {
		int index = -1;
		for (int i = 0; i < heap.size(); i++) {
			if (heap.get(i) == noAlvo) {
				index = i;
				break;
			}
		}
		if (index == -1)
			return;

		int prioridadeAntiga = noAlvo.getId();
		noAlvo.setId(novaPrioridade);

		// Se a prioridade diminuiu (ficou menor), pode ser necessário subir o nó
		if (novaPrioridade < prioridadeAntiga) {
			heapifyUp(index);
		} else {
			// Se a prioridade aumentou, pode ser necessário afundar o nó
			heapifyDown(index);
		}
		reconstruirArvore();
	}

	/**
	 * Move o nó em heap[index] para cima enquanto o pai tiver maior prioridade (no
	 * caso de min-heap, maior id).
	 */
	private void heapifyUp(int index) {
		while (index > 0) {
			int pai = (index - 1) / 2;
			if (heap.get(index).getId() < heap.get(pai).getId()) {
				swap(index, pai);
				index = pai;
			} else {
				break;
			}
		}
	}

	/**
	 * Move o nó em heap[index] para baixo caso algum dos filhos tenha prioridade
	 * menor.
	 */
	private void heapifyDown(int index) {
		int tamanho = heap.size();
		while (true) {
			int esq = 2 * index + 1;
			int dir = 2 * index + 2;
			int menor = index;

			if (esq < tamanho && heap.get(esq).getId() < heap.get(menor).getId()) {
				menor = esq;
			}

			if (dir < tamanho && heap.get(dir).getId() < heap.get(menor).getId()) {
				menor = dir;
			}

			if (menor != index) {
				swap(index, menor);
				index = menor;
			} else {
				break;
			}
		}
	}

	public Arvore<T> getArvore() {
		return arvore;
	}

	// Método auxiliar caso seja necessário buscar um nó pelo ID (não é muito
	// eficiente, mas demonstrativo)
	public No<T> buscarPorId(int id) {
		for (No<T> no : heap) {
			if (no.getId() == id) {
				return no;
			}
		}
		return null;
	}
}
