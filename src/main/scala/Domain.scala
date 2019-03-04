import Util.{Interpreter}

case class Address(id: Long, name : String, street: String, town: String, zip: String, company: String)

sealed trait AddressBookOp[A]
case class Put(address: Address) extends AddressBookOp[Unit]
case class Get(id : Long) extends AddressBookOp[Option[Address]]
case class Delete(address : Address) extends AddressBookOp[Unit]
case class Filter(foo : Address => Boolean) extends AddressBookOp[List[Address]]




object AddressBookUtil {


  // Create a common type for a AddressBookOp within a Free Container
  // This type is needed to generalize the return type of smart constructors
  type AddressBookOpDSL[A] = Free[AddressBookOp, A]

  // smart constructors
  def put(address: Address): AddressBookOpDSL[Unit] = Free.liftF(Put(address))
  def get(id: Long): AddressBookOpDSL[Option[Address]] = Free.liftF(Get(id))
  def delete(address: Address): AddressBookOpDSL[Unit] = Free.liftF(Delete(address))
  def filter(foo : Address => Boolean): AddressBookOpDSL[List[Address]] = Free.liftF(Filter(foo))


  val addressBookInterpreter = new Interpreter[AddressBookOp] {
    var m = scala.collection.Map[Long, Address]()
    def apply[A](program: AddressBookOp[A]): A = program match {
      case Put(address) =>
        println("Putting address with id: " + address.id)
        m = m + (address.id -> address)
      case Get(id) =>
        println("Getting address with id: " + id)
        m.get(id)
      case Delete(address) =>
        println("Deleting address with id: " + address.id)
        m = m - address.id
      case Filter(foo) => m.values.filter(foo).toList
    }
  }



}



