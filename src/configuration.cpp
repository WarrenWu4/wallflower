#include "configuration.hpp"

void Configuration::printConfiguration() {
  std::cout << "----Printing Directories----\n";
  for (std::string directory : this->directories) {
    std::cout << directory << "\n";
  }
  std::cout << "----Printing Image Data----\n";
  for (auto it = this->imageData.begin(); it != this->imageData.end(); it++) {
    std::cout << it->first << "," << modeToString[static_cast<int>(it->second)]<< "\n";
  }
}

Configuration::Configuration() {
  configurationPath = "/home/warrenwu/projects/wallflower/resources/configuration.txt";
  parseConfiguration();
  // WARNING: for testing purposes only
  // printConfiguration();
}

Configuration::~Configuration() {
  updateConfiguration();
}

void Configuration::parseConfiguration() {
  // TODO: add default behavior to create configuration file if it does not exist
  std::ifstream file(this->configurationPath);
  if (!file.is_open()) {
    std::cerr << "Error: unable to open file" << std::endl;
    return;
  }
  std::string line;
  bool reachedSeparator = false;
  while (std::getline(file, line)) {
    // WARNING: check blank line because std::getline() does not include newline at the end
    if (line == "") {
      reachedSeparator = true;
      continue;
    }
    if (!reachedSeparator) {
      this->directories.push_back(line);
    } else {
      std::stringstream ss(line);
      std::string key, value;

      if (std::getline(ss, key, ',') && std::getline(ss, value)) {
        imageData[key] = stringToMode.at(value);
      }
    }
  }
}

void Configuration::updateConfiguration() {
  std::cout << "updateConfiguration() is work in progress\n";
}
