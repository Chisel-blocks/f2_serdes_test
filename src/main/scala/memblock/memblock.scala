// See LICENSE for license details.
// Initially written by Marko Kosunen  20180429
package memblock 
import chisel3._
import chisel3.util._
import chisel3.experimental._
import dsptools._
import dsptools.numbers._
import f2_rx_dsp._
class memblock[T <:Data] (
        proto            : T,
        memsize          : Int=scala.math.pow(2,13).toInt
    ) extends Module {
    val io = IO( new Bundle { 
            val write_addr = Input(UInt(log2Ceil(memsize).W))
            val read_addr  = Input(UInt(log2Ceil(memsize).W))
            val read_val   = Output(proto)
            val write_val  = Input(proto)
    } )

    val mem =SyncReadMem(memsize, proto)
    val write_addr =RegInit(0.U(log2Ceil(memsize).W))
    val read_addr =RegInit(0.U(log2Ceil(memsize).W))
    //val write_en =RegInit(Bool())
    val write_val=RegInit(0.U.asTypeOf(io.write_val))
    val read_val =RegInit(0.U.asTypeOf(io.read_val))
    write_addr:=io.write_addr
    write_val:=io.write_val
    read_addr:=io.read_addr
    // Every clock cycle we write to memory, if write is enabled
    //when ( write_en===true.B) {
        mem.write(write_addr,write_val)
    //}.otherwise {
        read_val:=mem.read(read_addr)
    //}
  io.read_val:=read_val 
   
}
//This gives you verilog
object memblock extends App {
  val proto=new iofifosigs(n=16, users=4)
  chisel3.Driver.execute(args, () => new memblock(proto, memsize=scala.math.pow(2,13).toInt ))
}

