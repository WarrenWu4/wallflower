#pragma once

namespace Bouncer {
  /*
   * Checks if all requirements are met to run the program
     * If not, exits the program with an error message
     * Error also logged in log file if in production
   */
  void checkRequirements();
} // namespace Bouncer
