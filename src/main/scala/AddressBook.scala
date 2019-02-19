import AddressBookUtil.{AddressBookOpDSL, addressBookInterpreter, delete, put}
import Util.Interpreter

object AddressBook {

  def main(args: Array[String]): Unit = {

    val updateEntry: Address => AddressBookOpDSL[Unit] = address => for {
      _ <- delete(address) // delete old address
      _ <- put(address)
    } yield ()


    val newAddress = Address(1, "Santa Clause", "Snow Valley", "Santa's Town", "0", "Santa Deliveries")
    val updateEntryProgram : Interpreter[AddressBookOp] => Unit = Util.run(updateEntry(newAddress))

    updateEntryProgram(addressBookInterpreter)

    println(AddressBookUtil.addressBookInterpreter.m)
  }

}
