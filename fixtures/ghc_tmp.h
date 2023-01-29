#define VERSION_base "4.16.4.0"
#define MIN_VERSION_base(major1,major2,minor) (\
  (major1) <  4 || \
  (major1) == 4 && (major2) <  16 || \
  (major1) == 4 && (major2) == 16 && (minor) <= 4)

#define VERSION_data_default "0.7.1.1"
#define MIN_VERSION_data_default(major1,major2,minor) (\
  (major1) <  0 || \
  (major1) == 0 && (major2) <  7 || \
  (major1) == 0 && (major2) == 7 && (minor) <= 1)

#define VERSION_megaparsec "9.2.2"
#define MIN_VERSION_megaparsec(major1,major2,minor) (\
  (major1) <  9 || \
  (major1) == 9 && (major2) <  2 || \
  (major1) == 9 && (major2) == 2 && (minor) <= 2)

