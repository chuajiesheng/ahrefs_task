# Set OCamlMakefile to use
export OCAMLMAKEFILE = OCamlMakefile

# Export some common variable settings
export THREADS = yes
export ANNONATE = yes

# Define project "server"
define PROJ_server
  SOURCES = server.ml
  RESULT = server
endef
export PROJ_server

# Define project "client"
define PROJ_client
  SOURCES = client.ml
  RESULT = client
endef
export PROJ_client

# If the environment does not define subprojects to handle,
# then use "ex1 ex2" as default
ifndef SUBPROJS
  export SUBPROJS = server client
endif

# Default target to use
all:	bc

# Catch-all target will be applied to all subprojects automatically
%:
	@$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@
