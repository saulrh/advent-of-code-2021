cc_binary(
    name = "part1",
    deps = [
        "die",
        "player",
        "game",
        "@com_google_absl//absl/strings:str_format",
    ],
    srcs = ["part1.cc"],
)

cc_library(
    name = "die",
    srcs = ["die.cc"],
    hdrs = ["die.h"],
)

cc_test(
    name = "die-test",
    size = "small",
    srcs = ["die-test.cc"],
    deps = [
        "@com_google_googletest//:gtest_main",
        "die",
    ],
)

cc_library(
    name = "player",
    srcs = ["player.cc"],
    hdrs = ["player.h"],
)

cc_test(
    name = "player-test",
    size = "small",
    srcs = ["player-test.cc"],
    deps = [
        "@com_google_googletest//:gtest_main",
        "@com_google_absl//absl/random",
        "player",
    ],
)

cc_library(
    name = "game",
    srcs = ["game.cc"],
    hdrs = ["game.h"],
    deps = [
        "die",
        "player",
        "@com_google_absl//absl/types:span",
        "@com_google_absl//absl/types:optional",
        "@com_google_absl//absl/algorithm:container",
    ]
)

cc_test(
    name = "game-test",
    size = "small",
    srcs = ["game-test.cc"],
    deps = [
        "@com_google_googletest//:gtest_main",
        "game",
        "die",
        "player",
    ],
)

cc_binary(
    name = "part2",
    srcs = ["part2.cc"],
    deps = [
         "@com_google_absl//absl/container:flat_hash_map",
         "@com_google_absl//absl/container:flat_hash_set",
         "@com_google_absl//absl/hash",
         "@com_google_absl//absl/strings:str_format",
   ]
)
