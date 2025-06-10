NAME = ft_turing
SRC_DIR = src
OBJ_DIR = obj

SRCS = Printer.ml Main.ml
INTFS = 


ML_FILES   = $(addprefix $(SRC_DIR)/, $(SRCS))
MLI_FILES  = $(addprefix $(SRC_DIR)/, $(INTFS))
CMI_FILES  = $(patsubst $(SRC_DIR)/%.mli, $(OBJ_DIR)/%.cmi, $(MLI_FILES))
CMO_FILES  = $(patsubst $(SRC_DIR)/%.ml,  $(OBJ_DIR)/%.cmo, $(ML_FILES))
CMX_FILES  = $(patsubst $(SRC_DIR)/%.ml,  $(OBJ_DIR)/%.cmx, $(ML_FILES))

OCAMLC  = ocamlc
OCAMLOPT= ocamlopt
OCAMLFLAGS = -I $(OBJ_DIR) -I $(SRC_DIR) -thread



all: $(NAME)

$(OBJ_DIR):
	mkdir -p $(OBJ_DIR)


$(OBJ_DIR)/%.cmi: $(SRC_DIR)/%.mli | $(OBJ_DIR)
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

.PHONY: all clean fclean re