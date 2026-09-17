#include "WeightFactorAndRefDataCache.h"
#include "WeightFactorAndRefDataResolver.h"

#include <unordered_map>
#include <array>

static std::unordered_map<int, RefSpeciesData> refCache;

RefSpeciesData getCachedRefSpeciesData(int fiaSpcd)
{
    auto it = refCache.find(fiaSpcd);
    if (it != refCache.end()) {
        return it->second;
    }

    // Call your existing function
    RefSpeciesData result = getRefSpeciesData(fiaSpcd);

    refCache[fiaSpcd] = result;
    return result;
}


// --- Add this hash specialization (MSVC requires it) ---
namespace std {
    template<>
    struct hash<std::array<int, 3>> {
        size_t operator()(const std::array<int, 3>& a) const noexcept {
            size_t h1 = std::hash<int>{}(a[0]);
            size_t h2 = std::hash<int>{}(a[1]);
            size_t h3 = std::hash<int>{}(a[2]);
            return h1 ^ (h2 << 1) ^ (h3 << 2);
        }
    };
}
// --------------------------------------------------------

using SpeciesKey = std::array<int, 3>;

static std::unordered_map<SpeciesKey, WeightFactorAndRefData> speciesCache;

WeightFactorAndRefData getCachedSpeciesWtfactorAndRefData(
    int region,
    int forst,
    int fiaSpcd
)
{
    SpeciesKey key = { region, forst, fiaSpcd };

    auto it = speciesCache.find(key);
    if (it != speciesCache.end())
        return it->second;

    auto data = getSpeciesWtfactorAndRefData(region, forst, fiaSpcd);
    speciesCache[key] = data;
    return data;
}