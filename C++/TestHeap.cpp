//
// Criado por Vitor em 27/12/24.
//

#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>
#include <chrono>

#include "Heap/Heap.cpp"

/**
 * Divide a string de entrada digitada pelo usuário em um par <comando, valor>.
 *
 * Esta função recebe uma string de entrada que contém um comando e um valor
 * associado separados por espaços. Ela extrai o comando (chave) e o valor
 * e os retorna como um par.
 *
 * @param input A string que contém o comando digitado pelo usuário.
 * @return Um par contendo a chave (comando) e o valor associado.
 */
std::pair<std::string, std::string> splitCommand(const std::string& input) {
  std::istringstream iss(input);
  std::string key;
  std::string value;

  iss >> key;
  iss >> value;

  return {key, value};
}

/**
 * Lê uma lista de números de um arquivo e os armazena em um vetor.
 *
 * Esta função abre um arquivo cujo nome é fornecido, lê os números inteiros
 * contidos nele linha por linha ou separados por espaços e os armazena em
 * um vetor. Se o arquivo não puder ser aberto, uma mensagem de erro é exibida.
 *
 * @param filename O nome do arquivo a ser lido.
 * @return Um vetor contendo os números lidos do arquivo. Retorna um vetor vazio
 *         caso ocorra algum erro ao abrir o arquivo.
 */
std::vector<int> readListFromFile(const std::string& filename) {
  std::vector<int> numbers;
  std::ifstream file(filename);
  if (!file) {
    std::cerr << "ERRO: Não foi possível abrir o arquivo " << filename << "\n";
    return numbers;
  }

  int number;
  while (file >> number) {
    numbers.push_back(number);
  }

  file.close();
  return numbers;
}

int main() {
  std::string cliUser;                          //<- Entrada do usuário.

  Heap maxHeap(true);                           //<- Definição de uma maxHeap
  vector<int> heapOne;
  vector<int> heapTwo;
  vector<int> heap10K;
  vector<int> heap100K;
  vector<int> heap1M;

  const vector<int> valuesOne = {50, 48, 45, 29, 15, 35, 40, 27, 26, 14, 12, 33, 30, 37, 20, 21, 19, 25};
  const vector<int> valuesTwo = {7, 5, 12, 4, 8, 9, 3, 14, 1, 15};
  vector<int> list10K;
  vector<int> list100K;
  vector<int> list1M;

  while (cliUser != "exit") {
    std::cout << ">> ";
    std::getline(std::cin, cliUser);

    auto [key, value] = splitCommand(cliUser);

    maxHeap.makeHeap(heapOne, valuesOne);
    maxHeap.makeHeap(heapTwo, valuesTwo);

    if (key == "--help") {
      std::cout << "--help    : see helper. \n";
      std::cout << "--p1      : see change priority exemple #1 [50 to 10]\n";
      std::cout << "--p2      : see change priority exemple #2 [21 to 36]\n";
      std::cout << "--i1      : see insert exemple #1 [47]\n";
      std::cout << "--r1      : see remove exemple #1 [50]\n";
      std::cout << "--c1      : see construct exemple #1 [7 5 12 4 8 9 3 14 1 15]\n";
      std::cout << "--o1      : see ordenation exemple #1 [50 48 45 29 15 35 40 27 26 14 12 33 30 37 20 21 19 25]\n";
      std::cout << "--o2      : see ordenation exemple #1 [7 5 12 4 8 9 3 14 1 15]\n";
      std::cout << "--a       : see all ordenation cases\n";
    }

    /*
     * Executa o teste de alteração de prioridade 1.
     */
    if (key == "--p1") {
      std::cout << "Original heap: ";
      maxHeap.showHeap(heapOne);
      maxHeap.changePriority(heapOne, 1, 10);
      std::cout << "Change priority (50 to 10): ";
      maxHeap.showHeap(heapOne);
    }

    /*
     * Executa o teste de alteração de prioridade 2.
     */
    if (key == "--p2") {
      std::cout << "Original heap: ";
      maxHeap.showHeap(heapOne);
      maxHeap.changePriority(heapOne, 16, 36);
      std::cout << "Change priority (21 to 36): ";
      maxHeap.showHeap(heapOne);
    }

    /*
     * Executa o teste de inserção.
     */
    if (key == "--i1") {
      std::cout << "Original heap: ";
      maxHeap.showHeap(heapOne);
      maxHeap.insert(heapOne, 47);
      std::cout << "Insert [47]: ";
      maxHeap.showHeap(heapOne);
    }

    /*
     * Executa o teste de remoção.
     */
    if (key == "--r1") {
      std::cout << "Original heap: ";
      maxHeap.showHeap(heapOne);
      maxHeap.remove(heapOne);
      std::cout << "Remove [50]: ";
      maxHeap.showHeap(heapOne);
    }

    /*
     * Executa o de construção da heap.
     */
    if (key == "--c1") {
      std::cout << "Array with values: ";
      maxHeap.showHeap(valuesTwo);
      maxHeap.makeHeap(heapTwo, valuesTwo);
      std::cout << "Heap construct: ";
      maxHeap.showHeap(heapTwo);
    }

    /*
     * Executa a ordenação para a primeira heap.
     */
    if (key == "--o1") {
      std::cout << "Original heap: ";
      maxHeap.showHeap(heapOne);
      maxHeap.heapSort(heapOne);
      std::cout << "Sorted heap: ";
      maxHeap.showHeap(heapOne);
    }

    /*
     * Executa a ordenação para a segunda heap.
     */
    if (key == "--o2") {
      std::cout << "Original heap: ";
      maxHeap.showHeap(heapTwo);
      maxHeap.heapSort(heapTwo);
      std::cout << "Sorted heap: ";
      maxHeap.showHeap(heapTwo);
    }

    /*
     * Executa a ordenação das listas com 10k, 100k e 1M de elementos.
     * Além disso, computa o tempo de processamento de cada ordenação.
     */
    if (key == "--a") {
      list10K = readListFromFile("Heap/list_10k.txt");
      list100K = readListFromFile("Heap/list_100k.txt");
      list1M = readListFromFile("Heap/list_1M.txt");

      maxHeap.makeHeap(heap10K, list10K);
      maxHeap.makeHeap(heap100K, list100K);
      maxHeap.makeHeap(heap1M, list1M);

      auto start = std::chrono::high_resolution_clock::now();
      maxHeap.heapSort(heap10K);
      auto end = std::chrono::high_resolution_clock::now();
      std::cout << "Sort 10K elements in: "
                << std::chrono::duration<double>(end - start).count() << "s.\n";

      start = std::chrono::high_resolution_clock::now();
      maxHeap.heapSort(heap100K);
      end = std::chrono::high_resolution_clock::now();
      std::cout << "Sort 100k elements in: "
                << std::chrono::duration<double>(end - start).count() << "s.\n";

      start = std::chrono::high_resolution_clock::now();
      maxHeap.heapSort(heap1M);
      end = std::chrono::high_resolution_clock::now();
      std::cout << "Sort 1M elements in: "
                << std::chrono::duration<double>(end - start).count() << "s.\n";
    }
  }
  return 0;
}