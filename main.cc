
#include <iostream>
#include <iomanip>
#include <string_view>
#include <array>
#include <variant>
#include <vector>

#include <CL/sycl.hpp>
#include <EGL/egl.h>

#define ARRAY(...) []<class First, class... Rest>(First first, Rest... rest) noexcept { \
        return std::array<First, 1 + sizeof... (rest)>{first, rest...}; \
    }(__VA_ARGS__)

#define SYMTBL(...) detail::symtbl(#__VA_ARGS__, __VA_ARGS__).compile()

namespace std
{
    template <class Ch, class... Args>
    auto& operator<<(std::basic_ostream<Ch>& output, std::variant<Args...>& v) noexcept {
        return std::visit([&](auto& v) -> auto& { return output << v; }, v);
    }
} // ::std

namespace detail
{
    inline namespace borrowed_code
    {
        // https://stackoverflow.com/questions/62108100/variant-with-unique-types
        template <typename T, typename... Ts>
        struct filter_duplicates { using type = T; };
        template <template <typename...> class C, typename... Ts, typename U, typename... Us>
        struct filter_duplicates<C<Ts...>, U, Us...>
            : std::conditional_t<(std::is_same_v<U, Ts> || ...)
                                 , filter_duplicates<C<Ts...>, Us...>
                                 , filter_duplicates<C<Ts..., U>, Us...>> {};
        template <typename T>
        struct unique_variant;
        template <typename... Ts>
        struct unique_variant<std::variant<Ts...>> : filter_duplicates<std::variant<>, Ts...> {};
        template <typename T>
        using unique_variant_t = typename unique_variant<T>::type;
    }
    template <size_t N, class... Args>
    struct symtbl {
        using key_type = std::string_view;
        using mapped_type = unique_variant_t<std::variant<Args...>>;
        using value_type = std::pair<key_type, mapped_type>;
        std::string_view const symbols;
        std::array<mapped_type, sizeof... (Args)> values;
        constexpr symtbl(char const (&symbols)[N], Args... values) noexcept 
            : symbols(symbols)
            , values{values...}
            {
            }
        constexpr auto size() const noexcept { return sizeof... (Args); }
        struct iterator {
            symtbl const& src;
            mutable std::string_view::const_iterator cur;
            mutable size_t idx;
            constexpr iterator(symtbl const& src, auto cur) noexcept
                : src(src)
                , cur(cur)
                , idx(0)
                {
                }
            constexpr auto operator*() const noexcept {
                return std::pair{
                    std::string_view(cur, std::find(cur, src.symbols.end(), ',')),
                    src.values[idx],
                };
            }
            constexpr auto operator++() const noexcept {
                cur = std::find(cur, src.symbols.end(), ',');
                if (cur != src.symbols.end()) {
                    ++idx;
                    while (*cur == ' ' || *cur == ',') ++cur;
                }
                return *this;
            }
            constexpr friend bool operator!=(iterator lhs, iterator rhs) noexcept {
                return lhs.cur != rhs.cur;
            }
        };
        constexpr iterator begin() const noexcept { return {*this, symbols.begin()}; }
        constexpr iterator end() const noexcept { return {*this, symbols.end()}; }
        consteval auto compile() noexcept {
            std::array<value_type, sizeof... (Args)> compiled;
            size_t idx = 0;
            for (auto pair : *this) {
                compiled[idx++] = pair;
            }
            return compiled;
        }
    };
} // ::detail

int main() {
    if (auto display = eglGetDisplay(EGL_DEFAULT_DISPLAY)) {
        int major = 0;
        int minor = 0;
        if (eglInitialize(display, &major, &minor)) {
            std::cout << "EGL " << major << "." << minor << " initialized." << std::endl;
            for (auto [sym, val] : SYMTBL(EGL_CLIENT_APIS,
                                          EGL_EXTENSIONS,
                                          EGL_VENDOR,
                                          EGL_VERSION))
            {
                EGLint name = std::get<int>(val);
                std::cout << sym << '(' << name << "):\n\t";
                std::cout << eglQueryString(display, name) << std::endl;
            }
            std::cout << "-------------------------------------------------------" << std::endl;
            EGLint num_config = 0;
            if (eglGetConfigs(display, nullptr, 0, &num_config)) { 
                std::vector<EGLConfig> configs(num_config);
                if (eglGetConfigs(display, configs.data(), configs.size(), &num_config)) {
                    constexpr auto egl_attributes = SYMTBL(
                        EGL_BUFFER_SIZE,
                        EGL_RED_SIZE,
                        EGL_GREEN_SIZE,
                        EGL_BLUE_SIZE,
                        EGL_LUMINANCE_SIZE,
                        EGL_ALPHA_SIZE,
                        EGL_ALPHA_MASK_SIZE,
                        EGL_BIND_TO_TEXTURE_RGB,
                        EGL_BIND_TO_TEXTURE_RGBA,
                        EGL_COLOR_BUFFER_TYPE,
                        EGL_CONFIG_CAVEAT,
                        EGL_CONFIG_ID,
                        EGL_CONFORMANT,
                        EGL_DEPTH_SIZE,
                        EGL_LEVEL,
                        EGL_MAX_PBUFFER_WIDTH,
                        EGL_MAX_PBUFFER_HEIGHT,
                        EGL_MAX_PBUFFER_PIXELS,
                        EGL_MAX_SWAP_INTERVAL,
                        EGL_MIN_SWAP_INTERVAL,
                        EGL_NATIVE_RENDERABLE,
                        EGL_NATIVE_VISUAL_ID,
                        EGL_NATIVE_VISUAL_TYPE,
                        EGL_RENDERABLE_TYPE,
                        EGL_SAMPLE_BUFFERS,
                        EGL_SAMPLES,
                        EGL_STENCIL_SIZE,
                        EGL_SURFACE_TYPE,
                        EGL_TRANSPARENT_TYPE,
                        EGL_TRANSPARENT_RED_VALUE,
                        EGL_TRANSPARENT_GREEN_VALUE,
                        EGL_TRANSPARENT_BLUE_VALUE);
                    for (auto [sym, dummy] : egl_attributes) {
                        std::cout << sym << '\t';
                    }
                    std::cout << std::endl;
                    for (auto config : configs) {
                        EGLint value = 0;
                        for (auto [dummy, val] : egl_attributes) {
                            eglGetConfigAttrib(display, config, std::get<int>(val), &value);
                            std::cout << value << '\t';
                        }
                        std::cout << std::endl;
                    }
                }
            }
            std::cout << "-------------------------------------------------------" << std::endl;
            EGLConfig config;
            if (eglChooseConfig(display,
                                ARRAY(EGL_LEVEL, 0,
                                      EGL_SURFACE_TYPE, EGL_WINDOW_BIT,
                                      EGL_RED_SIZE, 8,
                                      EGL_GREEN_SIZE, 8,
                                      EGL_BLUE_SIZE, 8,
                                      EGL_ALPHA_SIZE, 8,
                                      EGL_RENDERABLE_TYPE, EGL_OPENGL_ES3_BIT,
                                      EGL_NONE).data(),
                                &config, 1, &num_config))
            {
                EGLint value;
                eglGetConfigAttrib(display, config, EGL_CONFIG_ID, &value);
                std::cout << value << std::endl;
            }
        }
        eglTerminate(display);
    }
    return 0;
}
