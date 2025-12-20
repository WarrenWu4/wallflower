#pragma once

#include <source_location>
#include <string>

/**
 * DEBUG -> Useful information helpful in debugging process
 *
 * WARNING -> Issue that does not critically impact program behavior
 *
 * ERROR -> Issue that results in unknown behaviors
 *
 * OK -> Successful execution of event
 *
 * FAIL -> Unsuccessful execution of event
 *
 *
 * FAIL vs. ERROR -- why have two erroneous labels?
 * FAIL is designed to be handled within the bounds of the program
 * ERROR is issues with external factors beyond the program
 */
enum class LogLabel { DEBUG, WARNING, ERROR, OK, FAIL };

namespace Logger {
void logMsg(LogLabel level, const std::string &msg,
            const std::source_location = std::source_location().current());
}
