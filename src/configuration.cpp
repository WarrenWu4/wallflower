#include "configuration.hpp"

void Configuration::printConfiguration() {
  std::cout << "----Printing Directories----\n";
  for (auto it = this->directories.begin(); it != this->directories.end(); it++) {
    std::cout << *it << "\n";
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
  std::cout << "Configuration destructor\n";
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
      this->directories.insert(line);
    } else {
      std::stringstream ss(line);
      std::string key, value;

      if (std::getline(ss, key, ',') && std::getline(ss, value)) {
        imageData[key] = stringToMode.at(value);
      }
    }
  }
  file.close();
}

void Configuration::updateConfiguration() {
  printConfiguration();
  std::ofstream file(this->configurationPath);
  if (!file.is_open()) {
    std::cerr << "Error: unable to open file" << std::endl;
    return;
  }
  for (auto it = directories.begin(); it != directories.end(); it++) {
    std::string directory = *it;
    file << directory << "\n";
  }
  file << "\n";
  for (auto it = imageData.begin(); it != imageData.end(); it++) {
    std::string path = (*it).first;
    std::string fitMode = modeToString.at(static_cast<int>((*it).second));
    file << path << "," << path << "\n";
  }
  file.close();
  std::cout << "Updating configuration\n";
}
