#ifndef THREAD_UTILS_H
#define THREAD_UTILS_H

#include <string>
#include <sys/time.h>
#include <cmath>
#include <iostream>

class ThreadExecutor {
public:
  ThreadExecutor(std::string cmd) : cmd(cmd) {}
  ~ThreadExecutor() {}
  void setInput(std::string input) {
    this->input = input;
  }
  void setTimeoutSec(double timeout) {
    timeout_sec = timeout;
    use_timeout = true;
  }
  void run();
  int getReturnCode() {return ReturnCode;}
  bool isTimedOut() {return TimedOut;}
  std::string getStdOut() {return stdout;}
  // TODO
  std::string getStdErr() {return stderr;}
  double getExecutionTime() {
    return UsedTime;
  }
  void handle_signal(int signal);

  void child(int p0[2], int p1[2], int p2[2]);
  void parent(int child_pid, int p0[2], int p1[2], int p2[2]);
  bool checkChildStatus(int child_pid);
  // helper functions to run and get result
  static void runForNothing(std::string cmd) {
    ThreadExecutor exe(cmd);
    exe.run();
  }
  static std::string runForOutput(std::string cmd) {
    ThreadExecutor exe(cmd);
    exe.run();
    return exe.getStdOut();
  }
  static std::string runForOutput(std::string cmd, double timeout) {
    ThreadExecutor exe(cmd);
    exe.setTimeoutSec(timeout);
    exe.run();
    return exe.getStdOut();
  }
private:
  std::string cmd;
  std::string input;
  // only >0 will take effect
  double timeout_sec = 0;
  bool use_timeout = false;
  // double remaining_timeout = 0;
  // struct itimerval *timer = {0};

  bool TimedOut = false;
  int ReturnCode = -1;
  std::string stdout;
  std::string stderr;
  double UsedTime = 0;
};


#endif /* THREAD_UTILS_H */
