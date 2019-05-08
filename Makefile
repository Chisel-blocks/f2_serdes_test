#Directories
VERILOGPATH= /tools/projects/fader2/users/mkosunen/fader2_TheSDK/TheSDK_generators/f2_dsp_and_serdes/f2_dsp/f2_serdes_test/verilog
SCALAPATH= /tools/projects/fader2/users/mkosunen/fader2_TheSDK/TheSDK_generators/f2_dsp_and_serdes/f2_dsp/f2_serdes_test/src/main/scala
#DEPDIR :=.depdir
#$(shell mkdir -p $(DEPDIR) >/dev/null)
$(shell mkdir -p $(VERILOGPATH) >/dev/null)
MODULES= f2_serdes_test

TARGETS = $(foreach name,$(MODULES), $(VERILOGPATH)/$(name).v)
#Commands
SBT=sbt -J-Xmx16G -J-Xss8M

TOUCH=touch -r
vpath %.scala $(SCALAPATH)/f2_serdes_test
.PHONY: all help clean $(MODULES)


all: $(TARGETS)

#Generate recipes for individual modules
f2_serdes_test: $(VERILOGPATH)/f2_serdes_test.v

#Template for conditional makes
#Figure out the naming conventions later. Package is lowercase, class equals main method
#Does not track dependencies from scala source
$(VERILOGPATH)/%.v : %.scala  
	$(eval package:=$(basename $(notdir $@)))
	$(eval class:=$(basename $(notdir $@)))
	$(eval testbenchfile:=$(dir $<)tb_$(notdir $<))
	$(SBT) 'runMain $(package).$(class) -td $(VERILOGPATH)' 
	@#Test if testbech generator exists and compile it
	@if [ -f $(testbenchfile) ]; then \
		$(SBT) 'runMain $(package).tb_$(class) -td $(VERILOGPATH)'; \
	fi
clean:
	rm -f $(VERILOGPATH)/*.v
	rm -f $(VERILOGPATH)/*.anno
	rm -f $(VERILOGPATH)/*.fir
	rm -rf $(VERILOGPATH)
	#rm -rf $(DEPDIR)

MMOD ?= f2_serdes_test
memmap:
	cp $(VERILOGPATH)/$(MMOD).v  $(VERILOGPATH)/$(MMOD)_unmapped.orig.v
	$(SBT) 'runMain $(MMOD).$(MMOD) -td $(VERILOGPATH) --infer-rw $(MMOD) --repl-seq-mem -c:$(MMOD):-o:$(VERILOGPATH)/$(MMOD).conf'

#Generate cleanup recipes for individual modules
.PHONY: clean_f2_serdes_test
clean_f2_serdes_test:
	rm -f $(VERILOGPATH)/f2_serdes_test.v
	rm -f $(VERILOGPATH)/f2_serdes_test.anno
	rm -f $(VERILOGPATH)/f2_serdes_test.fir
	rm -f $(VERILOGPATH)/f2_serdes_test_memmapped.conf
	rm -f $(VERILOGPATH)/f2_serdes_test_memmapped.v

help:
	@echo "configured modules are:";
	@for i in $(MODULES) ; do \
	   echo $$i; \
	done
