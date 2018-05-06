// See LICENSE for license details.
// Initially written by Marko Kosunen  20180429

package f2_serdes_test
import chisel3._
import chisel3.util._
import chisel3.experimental._
import dsptools._
import dsptools.numbers._
import f2_rx_dsp._
import edge_detector._
import memblock._ 

class serdes_test_scan_ios( val n: Int, val users: Int, val memsize: Int) 
    extends Bundle {
        val write_mode   = Input(UInt(2.W))
        val write_address= Input(UInt(log2Ceil(memsize).W))
        val write_value  = Input(new iofifosigs(n=n,users=users))
        val write_en     = Input(Bool())
        val read_mode    = Input(UInt(2.W))
        val read_address = Input(UInt(log2Ceil(memsize).W))
        val read_value   = Output(new iofifosigs(n=n,users=users))
        val read_en      = Input(Bool())
    }

class f2_serdes_test_io(
        val n        : Int=16,
        val users    : Int=4,
        val memsize  : Int=scala.math.pow(2,13).toInt
    ) extends Bundle {
    val scan         =  new serdes_test_scan_ios(n=n,users=users,memsize=memsize)
    val to_serdes    =  DecoupledIO( new iofifosigs(n=n,users=users))
    val from_serdes  =  Flipped(DecoupledIO( new iofifosigs(n=n,users=users)))
}

class f2_serdes_test (
        n                : Int=16, 
        users            : Int=4,
        memsize          : Int=scala.math.pow(2,13).toInt
    ) extends Module {
    val io = IO( 
        new f2_serdes_test_io(
            n         = n, 
            users     = users,
            memsize   = memsize 
        )
    )
    //val z = new usersigzeros(n=n, users=users)
    val userzero   = 0.U.asTypeOf(new usersigs(n=n,users=users))
    val udatazero  = 0.U.asTypeOf(userzero.data)
    val uindexzero = 0.U.asTypeOf(userzero.uindex)
    val iofifozero = 0.U.asTypeOf(new iofifosigs(n=n,users=users))
    val datazero   = 0.U.asTypeOf(iofifozero.data)
    val rxindexzero= 0.U.asTypeOf(iofifozero.rxindex)


    //Output defaults
    io.scan.read_value:=iofifozero
    io.to_serdes.bits:=iofifozero
    io.to_serdes.valid:=true.B
    io.from_serdes.ready:=false.B
  
    // Need a memory with write from scan, read to scan, and 
    // To map this to SRAM, write address must be syncroniozed
    // All addressing through write_addri, enables throuhg write_en
    val proto=new iofifosigs(n=16, users=4)
    val mem = Module (new memblock(proto,memsize=memsize)).io
    //Defaults
    mem.write_val:=iofifozero
    mem.write_addr:=0.U.asTypeOf(mem.write_addr)
    mem.read_addr:=0.U.asTypeOf(mem.read_addr)

    /**
     OBS: The read and write modes of the scan_test_mem may be discrepant
     It is up to programmer if the mode combinations are rational
    **/
   
    //Write mode mapping
    val zero :: scan :: fill :: loop :: flush:: Nil = Enum(5)

    val w_write_mode=Wire(UInt())
    val fill_edge= Module ( new edge_detector()).io
    val write_loop_edge= Module ( new edge_detector()).io
     
    //Defaults
    fill_edge.A:=false.B
    write_loop_edge.A:=false.B
    //Write mode mapping
    w_write_mode:=zero
    when (io.scan.write_mode===0.U) {
        w_write_mode := zero
    } .elsewhen (io.scan.write_mode === 1.U ) {
        w_write_mode := scan
    } .elsewhen (io.scan.write_mode===2.U ) {
        fill_edge.A:=true.B
    } .elsewhen (io.scan.write_mode===3.U ) {
        w_write_mode := loop 
    } .otherwise {
        w_write_mode := zero
    }
    
    // State registers
    val write_state= RegInit(zero) //state register
    val read_state= RegInit(zero)

    //Write state machine
    val write_count=RegInit(0.U(memsize.W))
    when( w_write_mode===fill ) { fill_edge.A===true.B }.otherwise { fill_edge.A===false.B } 
    when( w_write_mode===loop ) { 
        write_loop_edge.A===true.B 
    }.otherwise { 
        write_loop_edge.A===false.B 
    } 
   
    when (write_state===zero) {
        when { fill_edge.rising===true.B }{
            write_state:=fill
        }.elsewhen{ write_loop_edge.rising===true.B}{
            write_state:=loop
        }.otherwise{ 
            write_state:=w_write_mode
        }
        io.from_serdes.ready:=false.B
        write_count:=0.U(memsize.W)

    }.elsewhen(write_state===scan) {
        when( io.scan.write_en===true.B) { 
            mem.write_addr:=io.scan.write_address
            mem.write_val:=io.scan.write_value 
        }
        write_state:=w_write_mode
    }.elsewhen(write_state===fill) {
        io.from_serdes.ready:=true.B
        when ( write_count < memsize) {
            mem.write_addr:=write_count
            mem.write_val:=io.from_serdes.bits 
            write_count:=write_count+1.U
            write_state:=write_state
        }.otherwise {
            write_count:=0.U(memsize.W)
            write_state := zero //Return to zero state
        }
    }.elsewhen(write_state === loop ) {
        io.from_serdes.ready:=true.B
        when (read_state === loop) {
            when ( write_count < memsize) {
                io.to_serdes.valid:=RegNext(RegNext(true.B))
                mem.read_addr:=write_count+1.U
                io.to_serdes.bits:=mem.read_val
                mem.write_addr:=write_count
                mem.write_val:=io.from_serdes.bits 
                write_count:=write_count+1.U
            }.otherwise {
                io.to_serdes.valid:=RegNext(RegNext(true.B))
                mem.read_addr:=write_count+1.U
                io.to_serdes.bits:=mem.read_val
                mem.write_addr:=write_count
                mem.write_val:=io.from_serdes.bits 
                write_count:=0.U(memsize.W)
            }
        }.otherwise {
            when ( write_count < memsize) {
                mem.write_addr:=write_count
                mem.write_val:=io.from_serdes.bits 
                write_count:=write_count+1.U
            }.otherwise {
                write_count:=0.U(memsize.W)
                write_count:=write_count+1.U
            }
        }
        write_state := w_write_mode 
    }.otherwise{
            //We should not end up here
            write_count:=0.U(memsize.W)
            write_state := zero
            io.from_serdes.ready:=false.B
    }

    // Read mode starts here
    val w_read_mode=Wire(UInt())
    val flush_edge= Module ( new edge_detector()).io
    val read_loop_edge= Module ( new edge_detector()).io
    //Defaults
    flush_edge.A:=false.B
    read_loop_edge.A:=false.B
     
    //Read mode mapping
    when (io.scan.read_mode===0.U) {
        w_read_mode := zero
    } .elsewhen (io.scan.read_mode===1.U ) {
        w_read_mode := scan
    } .elsewhen (io.scan.read_mode===2.U ) {
        w_read_mode := flush
    } .elsewhen (io.scan.read_mode===3.U ) {
        w_read_mode := loop 
    } .otherwise {
        w_read_mode := zero
    }
    
    //Read state machine
    val read_count=RegInit(0.U(memsize.W))
    when( w_read_mode===flush ) { flush_edge.A===true.B }.otherwise { flush_edge.A===false.B } 
    when( w_read_mode===loop ) { read_loop_edge.A===true.B }.otherwise { read_loop_edge.A===false.B } 
    when (read_state===zero) {
        when { flush_edge.rising===true.B }{
            read_count:=0.U(memsize.W)
            read_state:=flush
        }.elsewhen{ read_loop_edge.rising===true.B}{
            read_count:=1.U(memsize.W)
            read_state:=loop
        }.otherwise{ 
            read_count:=0.U(memsize.W)
            read_state:=w_read_mode
            io.to_serdes.valid:=false.B
        }
    }.elsewhen(read_state===scan) {
        when( io.scan.read_en===true.B) { 
            mem.read_addr:=io.scan.read_address
            io.scan.read_value:=mem.read_val 
        }
        read_state:=read_state
    }.elsewhen(read_state===flush) {
        when ( read_count < memsize) {
            mem.read_addr:=read_count
            io.to_serdes.valid:=RegNext(RegNext(true.B))
            io.to_serdes.bits :=mem.read_val
            read_count:=read_count+1.U
            read_state:=read_state
        }.otherwise {
            read_count:=0.U(memsize.W)
            read_state := zero //return to zero state 
        }
    }.elsewhen(read_state === loop) {
        when(write_state===loop) {
            //both tasks taken into account in write loop
            read_count:=0.U(memsize.W)
            read_state := w_read_mode 
        }.otherwise {
            when ( read_count < memsize) {
                io.to_serdes.valid:=RegNext(RegNext(true.B))
                io.to_serdes.bits :=mem.read_val
                read_count:=read_count+1.U
                read_state:=read_state
            }.otherwise {
                io.to_serdes.valid:=RegNext(RegNext(true.B))
                io.to_serdes.bits :=mem.read_val
                read_count:=0.U(memsize.W)
                read_state:=read_state
            }
        }
    }.otherwise{
            read_count:=0.U(memsize.W)
            //We should not end up here
            read_state := zero
            io.to_serdes.valid:=false.B
    }
   
}
//This gives you verilog
object f2_serdes_test extends App {
  chisel3.Driver.execute(args, () => new f2_serdes_test(n=16, users=4, memsize=scala.math.pow(2,13).toInt ))
}


