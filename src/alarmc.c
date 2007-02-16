#ifdef unix
#include <unistd.h>
void alarmc(int *seconds){
  alarm(*seconds);
  return;
}
#endif    
    
    
    
