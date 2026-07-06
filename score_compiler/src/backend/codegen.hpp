#pragma once

#include "resolve/resolver.hpp"
#include <string>

[[nodiscard]] auto graph_to_json(const ExpandedGraph &graph) -> std::string;
