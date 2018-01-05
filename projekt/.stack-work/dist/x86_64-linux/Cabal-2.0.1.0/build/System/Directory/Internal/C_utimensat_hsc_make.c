#include "/home/mdronski/.stack/programs/x86_64-linux/ghc-8.2.2/lib/ghc-8.2.2/template-hsc.h"
#line 2 "C_utimensat.hsc"
#include <HsDirectoryConfig.h>
#line 3 "C_utimensat.hsc"
#ifdef HAVE_UTIMENSAT
#line 4 "C_utimensat.hsc"
#ifdef HAVE_FCNTL_H
#line 5 "C_utimensat.hsc"
#include <fcntl.h>
#line 6 "C_utimensat.hsc"
#endif 
#line 7 "C_utimensat.hsc"
#ifdef HAVE_TIME_H
#line 8 "C_utimensat.hsc"
#include <time.h>
#line 9 "C_utimensat.hsc"
#endif 
#line 10 "C_utimensat.hsc"
#ifdef HAVE_SYS_STAT_H
#line 11 "C_utimensat.hsc"
#include <sys/stat.h>
#line 12 "C_utimensat.hsc"
#endif 
#line 13 "C_utimensat.hsc"
#include <System/Directory/Internal/utility.h>
#line 47 "C_utimensat.hsc"
#endif 

int main (void)
{
#line 3 "C_utimensat.hsc"
#ifdef HAVE_UTIMENSAT
#line 4 "C_utimensat.hsc"
#ifdef HAVE_FCNTL_H
#line 6 "C_utimensat.hsc"
#endif 
#line 7 "C_utimensat.hsc"
#ifdef HAVE_TIME_H
#line 9 "C_utimensat.hsc"
#endif 
#line 10 "C_utimensat.hsc"
#ifdef HAVE_SYS_STAT_H
#line 12 "C_utimensat.hsc"
#endif 
#line 47 "C_utimensat.hsc"
#endif 
    hsc_line (1, "src/System/Directory/Internal/C_utimensat.hsc");
    hsc_fputs ("", hsc_stdout());
    hsc_fputs ("module System.Directory.Internal.C_utimensat where\n"
           "", hsc_stdout());
    hsc_fputs ("", hsc_stdout());
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_fputs ("", hsc_stdout());
#line 3 "C_utimensat.hsc"
#ifdef HAVE_UTIMENSAT
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (4, "src/System/Directory/Internal/C_utimensat.hsc");
    hsc_fputs ("", hsc_stdout());
#line 4 "C_utimensat.hsc"
#ifdef HAVE_FCNTL_H
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (5, "src/System/Directory/Internal/C_utimensat.hsc");
    hsc_fputs ("", hsc_stdout());
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_fputs ("", hsc_stdout());
#line 6 "C_utimensat.hsc"
#endif 
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (7, "src/System/Directory/Internal/C_utimensat.hsc");
    hsc_fputs ("", hsc_stdout());
#line 7 "C_utimensat.hsc"
#ifdef HAVE_TIME_H
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (8, "src/System/Directory/Internal/C_utimensat.hsc");
    hsc_fputs ("", hsc_stdout());
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_fputs ("", hsc_stdout());
#line 9 "C_utimensat.hsc"
#endif 
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (10, "src/System/Directory/Internal/C_utimensat.hsc");
    hsc_fputs ("", hsc_stdout());
#line 10 "C_utimensat.hsc"
#ifdef HAVE_SYS_STAT_H
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (11, "src/System/Directory/Internal/C_utimensat.hsc");
    hsc_fputs ("", hsc_stdout());
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_fputs ("", hsc_stdout());
#line 12 "C_utimensat.hsc"
#endif 
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (13, "src/System/Directory/Internal/C_utimensat.hsc");
    hsc_fputs ("", hsc_stdout());
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_fputs ("import Prelude ()\n"
           "import System.Directory.Internal.Prelude\n"
           "import Data.Time.Clock.POSIX (POSIXTime)\n"
           "\n"
           "data CTimeSpec = CTimeSpec EpochTime CLong\n"
           "\n"
           "instance Storable CTimeSpec where\n"
           "    sizeOf    _ = ", hsc_stdout());
#line 21 "C_utimensat.hsc"
    hsc_size (struct timespec);
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (22, "src/System/Directory/Internal/C_utimensat.hsc");
    hsc_fputs ("    -- workaround (hsc2hs for GHC < 8.0 doesn\'t support #{alignment ...})\n"
           "    alignment _ = ", hsc_stdout());
#line 23 "C_utimensat.hsc"
    hsc_size (char[alignof(struct timespec)] );
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (24, "src/System/Directory/Internal/C_utimensat.hsc");
    hsc_fputs ("    poke p (CTimeSpec sec nsec) = do\n"
           "      (", hsc_stdout());
#line 25 "C_utimensat.hsc"
    hsc_poke (struct timespec, tv_sec);
    hsc_fputs ("", hsc_stdout());
    hsc_fputs (")  p sec\n"
           "", hsc_stdout());
    hsc_line (26, "src/System/Directory/Internal/C_utimensat.hsc");
    hsc_fputs ("      (", hsc_stdout());
#line 26 "C_utimensat.hsc"
    hsc_poke (struct timespec, tv_nsec);
    hsc_fputs ("", hsc_stdout());
    hsc_fputs (") p nsec\n"
           "", hsc_stdout());
    hsc_line (27, "src/System/Directory/Internal/C_utimensat.hsc");
    hsc_fputs ("    peek p = do\n"
           "      sec  <- ", hsc_stdout());
#line 28 "C_utimensat.hsc"
    hsc_peek (struct timespec, tv_sec );
    hsc_fputs (" ", hsc_stdout());
    hsc_fputs ("p\n"
           "", hsc_stdout());
    hsc_line (29, "src/System/Directory/Internal/C_utimensat.hsc");
    hsc_fputs ("      nsec <- ", hsc_stdout());
#line 29 "C_utimensat.hsc"
    hsc_peek (struct timespec, tv_nsec);
    hsc_fputs (" ", hsc_stdout());
    hsc_fputs ("p\n"
           "", hsc_stdout());
    hsc_line (30, "src/System/Directory/Internal/C_utimensat.hsc");
    hsc_fputs ("      return (CTimeSpec sec nsec)\n"
           "\n"
           "c_AT_FDCWD :: CInt\n"
           "c_AT_FDCWD = (", hsc_stdout());
#line 33 "C_utimensat.hsc"
    hsc_const (AT_FDCWD);
    hsc_fputs ("", hsc_stdout());
    hsc_fputs (")\n"
           "", hsc_stdout());
    hsc_line (34, "src/System/Directory/Internal/C_utimensat.hsc");
    hsc_fputs ("\n"
           "utimeOmit :: CTimeSpec\n"
           "utimeOmit = CTimeSpec (CTime 0) (", hsc_stdout());
#line 36 "C_utimensat.hsc"
    hsc_const (UTIME_OMIT);
    hsc_fputs ("", hsc_stdout());
    hsc_fputs (")\n"
           "", hsc_stdout());
    hsc_line (37, "src/System/Directory/Internal/C_utimensat.hsc");
    hsc_fputs ("\n"
           "toCTimeSpec :: POSIXTime -> CTimeSpec\n"
           "toCTimeSpec t = CTimeSpec (CTime sec) (truncate $ 10 ^ (9 :: Int) * frac)\n"
           "  where\n"
           "    (sec,  frac)  = if frac\' < 0 then (sec\' - 1, frac\' + 1) else (sec\', frac\')\n"
           "    (sec\', frac\') = properFraction (toRational t)\n"
           "\n"
           "foreign import ccall \"utimensat\" c_utimensat\n"
           "  :: CInt -> CString -> Ptr CTimeSpec -> CInt -> IO CInt\n"
           "\n"
           "", hsc_stdout());
#line 47 "C_utimensat.hsc"
#endif 
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (48, "src/System/Directory/Internal/C_utimensat.hsc");
    hsc_fputs ("", hsc_stdout());
    return 0;
}
