#ifndef LOGGER_HPP
#define LOGGER_HPP

class Logger {
public:
    enum Level { DEBUG = 0, INFO = 1, WARN = 2, ERROR = 3 };

    virtual ~Logger() {}

    virtual void log(Level level, const char *message) = 0;
};

#endif
