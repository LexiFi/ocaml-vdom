DEPS = \
  split.js/dist/split.min.js \
  codemirror@5.65.13/lib/codemirror.css \
  codemirror@5.65.13/lib/codemirror.js \
  codemirror@5.65.13/mode/mllike/mllike.js \
  codemirror@5.65.13/addon/scroll/simplescrollbars.js \
  codemirror@5.65.13/addon/selection/active-line.js \
  codemirror@5.65.13/theme/zenburn.css

NOP :=
SPACE := $(NOP) $(NOP)
COMMA := ,

.PHONY: all
all:
	curl -OL https://unpkg.com/{$(subst $(SPACE),$(COMMA),$(DEPS))}
