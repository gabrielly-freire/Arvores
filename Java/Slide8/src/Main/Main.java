package Main;

import java.util.Scanner;

import Aplicacoes.Metodos;
import Entidades.Arvore;
import Entidades.No;

public class Main {
    public static void main(String[] args) {
        Arvore<String> arvore = new Arvore<>();
        Metodos<String> metodos = new Metodos<>();
        
        Scanner sc = new Scanner(System.in);

        int opcao = -1;
        while (opcao != 0) {
            System.out.println("1 - Inserir");
            System.out.println("2 - Remover");
            System.out.println("3 - Buscar");
            System.out.println("0 - Sair");
            opcao = sc.nextInt();
            sc.nextLine(); 

            switch (opcao) {
                case 1:
                    System.out.print("Digite o ID: ");
                    int idInsert = sc.nextInt(); sc.nextLine();
                    System.out.print("Digite o dado (string): ");
                    String dado = sc.nextLine();
                    metodos.inserir(arvore, idInsert, dado);
                    System.out.println("Inserido com sucesso.");
                    break;
                case 2:
                    System.out.print("Digite o ID a remover: ");
                    int idRemove = sc.nextInt(); sc.nextLine();
                    metodos.remover(arvore, idRemove);
                    System.out.println("Remoção concluída.");
                    break;
                case 3:
                    System.out.print("Digite o ID a buscar: ");
                    int idBusca = sc.nextInt(); sc.nextLine();
                    No<String> resultado = metodos.buscar(arvore, idBusca);
                    if (resultado == arvore.getNIL()) {
                        System.out.println("Não encontrado.");
                    } else {
                        System.out.println("Encontrado: ID = " + resultado.getId() + ", Dado = " + resultado.getDados());
                    }
                    break;
                case 0:
                    System.out.println("Saindo...");
                    break;
                default:
                    System.out.println("Opção inválida.");
            }
        }

        sc.close();
    }
}

