// time_t_decoder.cpp --- pretty print time_t values
// Author: Dan Harms <enniomore@icloud.com>
// Created: Thursday, July  2, 2015
// Modified Time-stamp: <2022-07-21 14:33:39 dharms>
// Modified by: Dan Harms

#include <iostream>
#include <iomanip>
#include <ctime>

int main(int argc, char* argv[])
{
   if (argc < 1)
   {
      return EXIT_FAILURE;
   }
   time_t when;
   if (argc == 1)
   {
      when = std::time(nullptr);
   }
   else
   {
      long long val = std::atoll(argv[1]);
      static const long long secs = 10000000000;
      static const long long nanos = 1000000000;
      if (val > secs)
      {
         val /= nanos;
      }
      when = val;
   }

   std::cout << std::put_time(std::gmtime(&when), "%c %Z")
             << " / "
             << std::put_time(std::localtime(&when), "%c %Z")
             << std::endl;

   return EXIT_SUCCESS;;
}

// code ends here
