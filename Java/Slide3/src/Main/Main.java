package Main;

import java.util.Scanner;

import Aplicacoes.Heap;
import Entidades.No;

public class Main {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        
        Heap<String> heap = new Heap<>();

        System.out.print("Quantos elementos deseja inserir na heap? ");
        int n = sc.nextInt();
        sc.nextLine(); // Consumir quebra de linha

        for (int i = 0; i < n; i++) {
            System.out.print("Digite o ID (inteiro) do elemento " + (i+1) + ": ");
            int id = sc.nextInt();
            sc.nextLine(); // Consumir quebra de linha
            
            System.out.print("Digite os dados (string) do elemento " + (i+1) + ": ");
            String dados = sc.nextLine();

            heap.inserir(id, dados);
        }

        // Exibir a heap após inserção
        System.out.println("Heap após inserção:");
        exibirHeap(heap);

        // Remover raiz se o usuário desejar
        System.out.print("Deseja remover a raiz? (s/n): ");
        char resp = sc.next().charAt(0);
        if (resp == 's' || resp == 'S') {
            String removido = heap.removerRaiz();
            System.out.println("Raiz removida: " + removido);
            System.out.println("Heap após remoção da raiz:");
            exibirHeap(heap);
        }

        // Alterar prioridade se o usuário desejar
        System.out.print("Deseja alterar a prioridade de um nó? (s/n): ");
        resp = sc.next().charAt(0);
        sc.nextLine(); // Consumir quebra de linha
        if (resp == 's' || resp == 'S') {
            System.out.print("Digite o ID atual do nó que deseja alterar: ");
            int idAtual = sc.nextInt();
            System.out.print("Digite a nova prioridade (novo ID) do nó: ");
            int novaPrioridade = sc.nextInt();

            No<String> noAlvo = heap.buscarPorId(idAtual);
            if (noAlvo != null) {
                heap.alterarPrioridade(noAlvo, novaPrioridade);
                System.out.println("Heap após alterar a prioridade:");
                exibirHeap(heap);
            } else {
                System.out.println("Nó com ID " + idAtual + " não encontrado.");
            }
        }

        sc.close();
    }

    // Função auxiliar para exibir a heap percorrendo a árvore (in-order)
    private static void exibirHeap(Heap<String> heap) {
        exibirNo(heap.getArvore().getRaiz());
        System.out.println();
    }

    private static void exibirNo(No<String> no) {
        if (no == null) return;
        exibirNo(no.getEsq());
        System.out.print("(" + no.getId() + ":" + no.getDados() + ") ");
        exibirNo(no.getDir());
    }
}
