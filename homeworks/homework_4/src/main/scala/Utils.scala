import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Utils {
  class PhoneBook(private val phoneNumbers: mutable.ListBuffer[String] = new ListBuffer[String]) {
    def insert(phoneNumber: String): Unit = phoneNumbers += phoneNumber
    def list(): List[String] = phoneNumbers.toList
    def delete(phoneNumber: String): Unit = phoneNumbers.filter(_ != phoneNumber)
  }

  def isValidPhoneNumber(phoneNumber: String): Boolean =
    phoneNumber.matches("^[\\+]?[(]?[0-9]{3}[)]?[-\\s\\.]?[0-9]{3}[-\\s\\.]?[0-9]{4,6}$")

  class SimplePhoneService(phoneBook: PhoneBook) {
    def findPhoneNumber(phoneNumber: String): String = {
      val filteredPhoneNumbers = phoneBook.list().filter(_ == phoneNumber)
      if (filteredPhoneNumbers.nonEmpty)
        filteredPhoneNumbers.head
      else
        null
    }

    def addPhoneToBase(phoneNumber: String): Unit = {
      if (isValidPhoneNumber(phoneNumber))
        phoneBook.insert(phoneNumber)
      else
        throw new InternalError("Invalid phone string")
    }

    def deletePhone(phoneNumber: String): Unit = phoneBook.delete(phoneNumber)
  }

  trait ChangePhoneService {
    def changePhone(oldPhoneNumber: String, newPhoneNumber: String): String
  }
}
