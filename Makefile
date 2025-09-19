NAME = ft_turing
SRC_DIR = src
INTF_DIR = intf
OBJ_DIR = obj
SWITCH_NAME=ft_turing_env
OCAML_VERSION=5.2.1

SRCS = Machine_error.ml Machine_validator.ml Machine.ml Machine_loader.ml Machine_printer.ml Runner.ml Main.ml
INTFS = Machine_error.mli Machine_validator.mli Machine.mli Machine_loader.mli Machine_printer.mli Runner.mli

ML_FILES = $(addprefix $(SRC_DIR)/, $(SRCS))
MLI_FILES = $(addprefix $(INTF_DIR)/, $(INTFS))
CMI_FILES = $(patsubst $(INTF_DIR)/%.mli, $(OBJ_DIR)/%.cmi, $(MLI_FILES))
CMO_FILES = $(patsubst $(SRC_DIR)/%.ml,  $(OBJ_DIR)/%.cmo, $(ML_FILES))
CMX_FILES = $(patsubst $(SRC_DIR)/%.ml,  $(OBJ_DIR)/%.cmx, $(ML_FILES))

OCAMLC = ocamlfind ocamlc
OCAMLOPT = ocamlfind ocamlopt
PKGS = yojson
OCAMLFLAGS = -I $(OBJ_DIR) -I $(SRC_DIR) -thread -package $(PKGS) -linkpkg

all: setup_env $(NAME)

$(OBJ_DIR):
	mkdir -p $(OBJ_DIR)


$(OBJ_DIR)/%.cmi: $(INTF_DIR)/%.mli | $(OBJ_DIR)
	$(OCAMLC) $(OCAMLFLAGS) -c $< -o $@

$(OBJ_DIR)/%.cmo: $(SRC_DIR)/%.ml | $(OBJ_DIR)
	$(OCAMLC) $(OCAMLFLAGS) -c $< -o $@

$(OBJ_DIR)/%.cmx: $(SRC_DIR)/%.ml | $(OBJ_DIR)
	$(OCAMLOPT) $(OCAMLFLAGS) -c $< -o $@


$(NAME).byte: $(CMI_FILES) $(CMO_FILES)
	$(OCAMLC) -o $@ $(OCAMLFLAGS) $(CMO_FILES)

$(NAME).opt: $(CMI_FILES) $(CMX_FILES)
	$(OCAMLOPT) -o $@ $(OCAMLFLAGS) $(CMX_FILES)

$(NAME): $(NAME).opt
	@ln -sf $(NAME).opt $(NAME)


clean:
	rm -rf $(OBJ_DIR)

fclean: clean
	rm -f $(NAME) $(NAME).opt $(NAME).byte

re: fclean all

setup_env:
	@if ! opam switch list --short | grep -qx "$(SWITCH_NAME)"; then \
		opam update -y && \
		opam switch create $(SWITCH_NAME) $(OCAML_VERSION); \
		eval $$(opam env --switch=$(SWITCH_NAME)) && \
		opam install ocamlfind $(PKGS) --yes; \
	fi

delete_env:
	@if opam switch list --short | grep -qx "$(SWITCH_NAME)"; then \
		echo "==> removing switch $(SWITCH_NAME)"; \
		opam switch remove $(SWITCH_NAME) --yes; \
	else \
		echo "==> switch $(SWITCH_NAME) does not exist, nothing to remove"; \
	fi

.PHONY: all clean fclean re setup_env delete_env