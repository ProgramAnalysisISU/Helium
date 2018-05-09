#include "helium/utils/Utils.h"
#include "helium/utils/ThreadUtils.h"
#include <cstring>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <stdlib.h>

#include <cstdio>
#include <iostream>
#include <memory>
#include <stdexcept>
#include <string>

#include <fcntl.h>

#include <gtest/gtest.h>

#define TIMEOUT_RET_CODE 1234

// FIXME: Since I changed the timeout to be microseconds, all the commands use this need to change the time accordingly.
// FIXME: I changed exec to use execvp, and added another exec_sh to use sh -c version, which support output redirect.
// Thus, update accordingly.

/**
 * Is there a way to pass arbitrary commands? I don't think so, the redirect is handled by shel. So some shell library shall be used, e.g. bash, readline.
 * Another alternative: boost thread library.
 * @param [out] argv NULL terminated array
 */

static int split_cmd(const char *scon, int size, char** &argv) {
  char *s = strdup(scon);
  char *tok = strtok(s, " ");
  argv = (char**)malloc(size*sizeof(char*));
  int argc = 0;
  while (tok) {
    argv[argc] = (char*)malloc(strlen(tok)+1);
    strcpy(argv[argc], tok);
    tok = strtok(NULL, " ");
    argc++;
  }
  argv[argc] = NULL;
  return argc;
}

void ThreadExecutor::handle_signal(int signal) {
  // should be alarm
  if (signal != SIGALRM) {
    std::cerr << "Wrong signal: " << signal << "\n";
  }
  // timeout
  TimedOut = true;
}


void ThreadExecutor::child(int p0[2], int p1[2], int p2[2]) {
  // children
  if (dup2(p0[0], STDIN_FILENO) == -1 // stdin
      || dup2(p1[1], STDOUT_FILENO) == -1 // stdout
      || dup2(p2[1], STDERR_FILENO) == -1 // stderr
      ) {
    perror("dup error");
    exit(1);
  }

  // direct stderr to /dev/null
  // int null_fd = open("/dev/null", O_RDONLY);
  // dup2(null_fd, STDERR_FILENO);
  close(p0[0]);
  close(p0[1]);
  close(p1[0]);
  close(p1[1]);
  close(p2[0]);
  close(p2[1]);
  // prepare the command
  char **argv = NULL;
  // the argv is malloc-ed, but anyway the process will exit, it will be released
  split_cmd(cmd.c_str(), std::count(cmd.begin(), cmd.end(), ' '), argv);
  execvp(argv[0], argv);
  perror(cmd.c_str());
  exit(1);
}

/**
 * return true if child exited
 */
bool ThreadExecutor::checkChildStatus(int child_pid) {
  int status;
  int reportpid = waitpid(child_pid, &status, WNOHANG);
  if (reportpid == -1) {
    // error
    return false;
  } else if (reportpid == 0) {
    // no
    return false;
  } else if (reportpid == child_pid) {
    // exited
    if (WIFEXITED(status)) {
      ReturnCode = WEXITSTATUS(status);
    } else if (WIFSIGNALED(status)) {
      // assert(false);
    } else {
      // assert(false);
    }
    return true;
  } else {
    return false;
  }
}

void ThreadExecutor::parent(int child_pid, int p0[2], int p1[2], int p2[2]) {
  close(p0[0]);
  close(p1[1]);
  // write to child's input
  // FIXME this might block
  // std::cout << "writing .." << input << "\n";
  write(p0[1], input.c_str(), strlen(input.c_str()));
  // std::cout << "done" << "\n";
  close(p0[1]);

  struct timeval timeout;
  timeout.tv_sec = floor(timeout_sec);
  timeout.tv_usec = (timeout_sec - timeout.tv_sec) * 1000 * 1000;

  // std::cout << "Timeout set to: " << timeout.tv_sec << " " << timeout.tv_usec << "\n";
  
  struct timeval begin_timer, end_timer;
  gettimeofday(&begin_timer, NULL);
  
  while (true) {
    struct timeval time1, time2;
    // std::cout << "======= Remaining: " << timeout.tv_sec << " " << timeout.tv_usec << "\n";
    gettimeofday(&time1, NULL);

    if (checkChildStatus(child_pid)) {
      break;
    }
    
    fd_set set;
    FD_ZERO(&set);
    FD_SET(p1[0], &set);
    FD_SET(p2[0], &set);
    
    int result;
    if (use_timeout) {
      struct timeval tmp_timeout = timeout;
      result = select(FD_SETSIZE, &set, NULL, NULL, &tmp_timeout);
    } else {
      result = select(FD_SETSIZE, &set, NULL, NULL, NULL);
    }
    
    
    if (result == 0) {
      // timeout
      // std::cout << "Timed out. Killing child process .." << "\n";
      TimedOut = true;
      kill(child_pid, SIGKILL);
      checkChildStatus(child_pid);
      break;
    } else if (result > 0) {
      // read
      if (FD_ISSET(p1[0], &set)) {
        char buf[BUFSIZ];
        int nread = read(p1[0], buf, sizeof(buf));
        if (nread == -1) perror("read");
        else if (nread == 0) {
          // std::cout << "0 bytes read" << "\n";
        } else {
          // std::cout << "Total read: " << nread << "\n";
          // std::cout << "Current stdcout: " << stdout << "\n";
          stdout.append(buf, nread);
        }
      }
      if (FD_ISSET(p2[0], &set)) {
        char buf[BUFSIZ];
        int nread = read(p2[0], buf, sizeof(buf));
        if (nread == -1) perror("read");
        else if (nread == 0) {
        } else {
          stderr.append(buf, nread);
        }
      }
    }

    gettimeofday(&time2, NULL);
    timersub(&time2, &time1, &time1);
    timersub(&timeout, &time1, &timeout);
  }
  
  close(p1[0]);

  gettimeofday(&end_timer, NULL);
  timersub(&end_timer, &begin_timer, &end_timer);
  UsedTime = end_timer.tv_sec + (double)end_timer.tv_usec / 1000.0 / 1000.0;
}


void ThreadExecutor::run() {
  // create pipe
  int p0[2], p1[2], p2[2];
  if (pipe(p0) == -1
      || pipe(p1) == -1
      || pipe(p2) == -1
      ) {
    perror("pipe");
    exit(1);
  }
  pid_t pid;
  pid = fork();
  if (pid<0) {
    perror("fork");
    exit(1);
  }
  if (pid == 0) {
    child(p0, p1, p2);
  }
  // parent
  parent(pid, p0, p1, p2);
}


TEST(ThreadTest, ThreadExecutorTest) {

  std::string cmd;
  cmd = "/home/hebi/github/helium/test/script/a.sh";
  cmd = "echo hello";
  cmd = "cat";
  ThreadExecutor exe(cmd);
  exe.setInput("world");
  exe.setTimeoutSec(0.5);
  exe.run();

  std::cout << "=== timeout?: " << exe.isTimedOut() << "\n";
  std::cout << "=== return code: " << exe.getReturnCode() << "\n";
  std::cout << "=== stdout: " << exe.getStdOut() << "\n";
  // std::cout << "stderr: " << exe.getStdErr() << "\n";
  std::cout << "=== Execution time: " << exe.getExecutionTime() << "\n";
  // EXPECT_EQ(exe.getReturnCode(), 0);
  // EXPECT_FALSE(exe.isTimedOut());
  // EXPECT_EQ(exe.getStdOut(), "hello");
  // EXPECT_EQ(exe.getStdErr(), "");

  // std::string out = utils::new_exec(cmd.c_str());
  // EXPECT_EQ(out, "hello");
}
