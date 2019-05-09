// See LICENSE for license details.
// Initially written by Marko Kosunen  20180429

package f2_serdes_test
import chisel3._
import chisel3.util._
import chisel3.experimental._
import dsptools._
import dsptools.numbers._
import freechips.rocketchip.util._
import f2_signal_definitions.{usersigs, iofifosigs}
import edge_detector._
import memblock._

class serdes_test_scan_ios[T<: Data]( proto: T, val memsize: Int)
    extends Bundle {
        val write_mode   = Input(UInt(2.W))
        val write_address= Input(UInt(log2Ceil(memsize).W))
        val write_value  = Input(proto)
        val write_en     = Input(Bool())
        val read_mode    = Input(UInt(2.W))
        val read_address = Input(UInt(log2Ceil(memsize).W))
        val read_value   = Output(proto)
        val read_en      = Input(Bool())
    }

class f2_serdes_test_io[T <: Data](
        proto        : T,
        val memsize  : Int=scala.math.pow(2,13).toInt
    ) extends Bundle {
    val scan         =  new serdes_test_scan_ios(proto,memsize=memsize)
    val to_serdes    =  DecoupledIO(proto)
    val from_serdes  =  Flipped(DecoupledIO(proto))
    override def cloneType = (new f2_serdes_test_io(proto.cloneType,memsize)).asInstanceOf[this.type]
}

class memproto[ T <: Data](proto: T, val zpad: Int)
    extends Bundle {
        val signal=proto
        val zeros=UInt(zpad.W)
        override def cloneType = (new memproto(proto.cloneType,zpad)).asInstanceOf[this.type]
    }

class f2_serdes_test[T <:Data] (
        proto            : T,
        n                : Int=16,
        users            : Int=4,
        fifodepth        : Int=16,
        memsize          : Int=scala.math.pow(2,13).toInt
    ) extends Module {
    val io = IO(
        new f2_serdes_test_io(
            proto     = proto,
            memsize   = memsize
        )
    )
    //val z = new usersigzeros(n=n, users=users)
    val userzero   = 0.U.asTypeOf(new usersigs(n=n,users=users))
    val udatazero  = 0.U.asTypeOf(userzero.udata)
    val uindexzero = 0.U.asTypeOf(userzero.uindex)
    val iofifozero = 0.U.asTypeOf(new iofifosigs(n=n,users=users))
    val datazero   = 0.U.asTypeOf(iofifozero.data)
    val rxindexzero= 0.U.asTypeOf(iofifozero.rxindex)


    //These are just to decouple the IO
    val infifo = Module(new AsyncQueue(proto.cloneType,depth=fifodepth)).io
    infifo.enq<>io.from_serdes
    infifo.deq_clock:=clock
    infifo.enq_clock:=clock
    infifo.enq_reset:=reset
    infifo.deq_reset:=reset
    //Tis is to ensure that buffer is never full
    //Dropping of samples handled by valid
    val outfifo = Module(new AsyncQueue(proto.cloneType,depth=fifodepth)).io
    outfifo.deq<>io.to_serdes
    outfifo.enq_clock:=clock
    outfifo.deq_clock:=clock
    outfifo.deq_reset:=reset
    outfifo.enq_reset:=reset

    //Output defaults
    io.scan.read_value:=iofifozero
    outfifo.enq.bits:=iofifozero
    outfifo.enq.valid:=true.B
    infifo.deq.ready:=true.B

    // Need a memory with write from scan, read to scan, and
    // To map this to SRAM, write address must be syncroniozed
    // All addressing through write_addri, enables throuhg write_en
    //val proto=new iofifosigs(n=16, users=4)
    // We zpad the output in order to get MULTIPLE OF 37 which
    // is one of the memory output widths available
    // users*((16I+16Q)+4userindex)+2rxindex+14=592=16*37
    val memproto= new memproto(proto=proto.cloneType, zpad=14)
    val mem = Module (new memblock(proto=memproto.cloneType,memsize=memsize)).io
    //Defaults
    mem.write_val.zeros:=0.U.asTypeOf(mem.write_val.zeros.cloneType)
    mem.write_en:=false.B
    mem.write_val.signal:=iofifozero
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
        w_write_mode:=fill
    } .elsewhen (io.scan.write_mode===3.U ) {
        w_write_mode := loop
    } .otherwise {
        w_write_mode := zero
    }

    // State registers
    val write_state= RegInit(zero) //state register
    val read_state= RegInit(zero)

    //Write state machine
    val write_count=RegInit(0.U(log2Ceil(memsize).W))
    when( w_write_mode===fill ) { fill_edge.A:=true.B }.otherwise { fill_edge.A:=false.B }
    when( w_write_mode===loop ) {
        write_loop_edge.A:=true.B
    }.otherwise {
        write_loop_edge.A:=false.B
    }

    when (write_state===zero) {
        when { fill_edge.rising===true.B }{
            write_state:=fill
        }.elsewhen{ write_loop_edge.rising===true.B}{
            write_state:=loop
        }.elsewhen{w_write_mode===scan } {
            write_state:=scan 
        }.otherwise {
            write_state:=zero 
        }
        //infifo.deq.ready:=false.B
        write_count:=0.U

    }.elsewhen(write_state===scan) {
        when( io.scan.write_en===true.B) {
            mem.write_en:=true.B
            mem.write_addr:=io.scan.write_address
            mem.write_val.signal:=io.scan.write_value
        }
        when ( w_write_mode === scan ){
            write_state:=scan
        }.otherwise {
            write_state:=zero
        }
    }.elsewhen(write_state===fill) {
        //infifo.deq.ready:=true.B
        when ( (write_count < memsize) && infifo.deq.valid===true.B ) {
            mem.write_en:=true.B
            mem.write_addr:=write_count
            mem.write_val.signal:=infifo.deq.bits
            when ( write_count === memsize.asUInt-1.U) {
                write_count:=0.U
                write_state:=zero
            }.otherwise {
                write_state:=fill
                write_count:=write_count+1.U
            }
        }.otherwise {
            write_count:=0.U
            write_state := zero //Return to zero state after fill
                               // or if fill fails
        }
    }.elsewhen(write_state === loop ) {
        when ( w_write_mode === loop ){
            write_state:=loop
        }.otherwise {
            write_state:=zero
        }
        //cant't write faster than we read
        when (read_state === loop) {
            //read state update by read state machine
            when ( (write_count < memsize) 
                    && (outfifo.enq.ready===true.B) 
                    && (infifo.deq.valid===true.B)
                ) {
                when(write_count === memsize.asUInt-1.U) { 
                    outfifo.enq.valid:=RegNext(RegNext(true.B))
                    mem.read_addr:=write_count-1.U
                    outfifo.enq.bits:=mem.read_val.signal
                    mem.write_en:=true.B
                    mem.write_addr:=write_count
                    mem.write_val.signal:=infifo.deq.bits
                    write_count:=0.U
                }.otherwise {
                    outfifo.enq.valid:=RegNext(RegNext(true.B))
                    mem.read_addr:=write_count-1.U
                    outfifo.enq.bits:=mem.read_val.signal
                    mem.write_en:=true.B
                    mem.write_addr:=write_count
                    mem.write_val.signal:=infifo.deq.bits
                    write_count:=write_count+1.U
                }
            }.otherwise {
                //Loop does not progreess if fifo not ready
                // Safe, because we can exit the state by
                // changing w_write_state
                outfifo.enq.valid:=RegNext(RegNext(true.B))
                mem.read_addr:=write_count-1.U
                outfifo.enq.bits:=mem.read_val.signal
                mem.write_en:=true.B
                mem.write_addr:=write_count
                mem.write_val.signal:=infifo.deq.bits
                write_count:=write_count
            }
        }.otherwise {
            // Write looping, read not looping
            when ( (write_count < memsize ) 
                    && (infifo.deq.valid===true.B)
                ) {
                when(write_count === memsize.asUInt-1.U) { 
                    mem.write_en:=true.B
                    mem.write_addr:=write_count
                    mem.write_val.signal:=infifo.deq.bits
                    write_count:=0.U
                }.otherwise {
                    mem.write_en:=true.B
                    mem.write_addr:=write_count
                    mem.write_val.signal:=infifo.deq.bits
                    write_count:=write_count+1.U
                }
            }.otherwise {
                //Loop does not progreess if fifo not ready
                // Safe, because we can exit the state by
                // changing w_write_state
                //Do not write or increase address if dequeue is not valid
                write_count:=write_count
            }
        }
    }.otherwise{
            //We should not end up here
            // Default to zero state
            write_count:=0.U
            write_state := zero
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
    val read_count=RegInit(0.U(log2Ceil(memsize).W))
    when( w_read_mode===flush ) { flush_edge.A:=true.B }.otherwise { flush_edge.A:=false.B }
    when( w_read_mode===loop ) { read_loop_edge.A:=true.B }.otherwise { read_loop_edge.A:=false.B }
    when (read_state===zero) {
        when { flush_edge.rising===true.B }{
            read_count:=0.U
            read_state:=flush
        }.elsewhen{ read_loop_edge.rising===true.B}{
            read_count:=0.U
            read_state:=loop
        }.elsewhen{ w_read_mode===scan}{
            read_state:=scan
        }.otherwise{
            read_count:=0.U
            read_state:=zero
            outfifo.enq.valid:=false.B
        }
    }.elsewhen(read_state===scan) {
        when( io.scan.read_en===true.B) {
            mem.read_addr:=io.scan.read_address
            io.scan.read_value:=mem.read_val.signal
        }
        when (w_read_mode === scan) {
            read_state:=scan
        }.otherwise {
            read_state:=zero
        }

    }.elsewhen(read_state===flush) {
        when ((read_count < memsize)
                && outfifo.enq.ready
            ) {
            when ( read_count === memsize.asUInt-1.U ) {
                mem.read_addr:=read_count
                outfifo.enq.valid:=RegNext(RegNext(true.B))
                outfifo.enq.bits :=mem.read_val.signal
                read_count:=0.U
                read_state:=zero
            }.otherwise{
                mem.read_addr:=read_count
                outfifo.enq.valid:=RegNext(RegNext(true.B))
                outfifo.enq.bits :=mem.read_val.signal
                read_count:=read_count+1.U
                read_state:=flush
            }
        }.otherwise {
            //The flush should be clean. if not, return to zero state
            read_count:=0.U
            read_state := zero //return to zero state
        }
    }.elsewhen( read_state === loop) {
        when (w_read_mode === loop) {
            read_state:=loop
        }.otherwise {
            read_state:=zero
        }
        when(write_state===loop) {
            //both addresses taken into account in write loop
            read_count:=0.U
        }.otherwise {
            when ( (read_count < memsize) 
                && outfifo.enq.ready
                ) {
                when ( read_count === memsize.asUInt-1.U ) {
                    mem.read_addr:=read_count
                    outfifo.enq.valid:=RegNext(RegNext(true.B))
                    outfifo.enq.bits :=mem.read_val.signal
                    read_count:=0.U
                }.otherwise {
                    mem.read_addr:=read_count
                    outfifo.enq.valid:=RegNext(RegNext(true.B))
                    outfifo.enq.bits :=mem.read_val.signal
                    read_count:=read_count+1.U
                }
            }.otherwise {
                //Do not increase counter if fifo not ready
                read_count:=read_count
            }
        }
    }.otherwise{
            // Otherwise we return to zero state
            read_count:=0.U
            read_state := zero
            outfifo.enq.valid:=false.B
    }
}
//This gives you verilog
object f2_serdes_test extends App {
  val n=16
  val users=16
  val fifodepth=16
  val proto=new iofifosigs(n=n, users=users)
  chisel3.Driver.execute(args, () 
      => new f2_serdes_test(proto=proto.cloneType, 
          n=n, 
          users=users, fifodepth=fifodepth, 
          memsize=scala.math.pow(2,13).toInt 
          )
      )
}


