#include <iostream>
#include <fstream>
#include <vector>
#include <random>
#include <string>

/**
 * Gera uma lista de números aleatórios e os salva em um arquivo.
 *
 * Esta função cria uma lista de números inteiros aleatórios no intervalo de
 * 0 a 10.000 e os grava em um arquivo especificado. A quantidade de números
 * gerados é determinada pelo parâmetro 'size'. Se o arquivo não puder ser
 * aberto, uma mensagem de erro é exibida.
 *
 * @param filename O nome do arquivo onde a lista será salva.
 * @param size O número de elementos a serem gerados e salvos.
 */
void generateAndSaveList(const std::string& filename, size_t size) {
  std::random_device rd;
  std::mt19937 gen(rd());
  std::uniform_int_distribution<> dis(0, 10000);

  std::ofstream file(filename);
  if (!file) {
    std::cerr << "ERROR: " << filename << "\n";
    return;
  }

  for (size_t i = 0; i < size; ++i) { file << dis(gen) << "\n"; }

  file.close();
  std::cout << "Saved in " << filename << "\n";
}

int main() {
  generateAndSaveList("Heap/list_10k.txt", 10000);
  generateAndSaveList("Heap/list_100k.txt", 100000);
  generateAndSaveList("Heap/list_1M.txt", 1000000);

  return 0;
}
