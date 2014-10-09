-----------------------------------------------------------
-- Directory structure                                   -- 
-----------------------------------------------------------

- Bitvis_Utility_Library_<ReleaseNumber> : Top level directory
  
  - bitvis_util     : Bitvis Utility Library
    - doc           : Quick Reference
    - script        : Compile scripts for Bitvis Utility Library
    - sim           : Simulation directory with a Modelsim project file. Simulation log files will be generated in this directory during simulation.  
    - src93         : Source code for the VHDL93 version of Bitvis Utility Library
    - src2002       : Source code for the VHDL2002 version of Bitvis Utility Library
    - src2008       : Source code for the VHDL2008 version of Bitvis Utility Library

  - bitvis_irqc     : An example Design Under Test (DUT) delivered with Bitvis Utility Library in order to illustrate usage and debugging support provided by the library.
    - doc           : Documentation
    - script        : IRQC compile scripts
    - sim           : Simulation directory with a Modelsim project file. Simulation log files will be generated in this directory during simulation.  
    - src           : IRQC source code
    - tb            : IRQC Test bench

  - bitvis_vip_sbi  : An example BFM that uses Bitvis Utility Library
    - script        : Compile script
    - src           : SBI BFM source code

  - x_ieee_proposed : The IEEE Proposed library, used by the VHDL2002 and VHDL93 versions of Bitvis Utility Library. 
                      This library allows 2008-functionality to be used in simulators not supporting VHDL2008.
