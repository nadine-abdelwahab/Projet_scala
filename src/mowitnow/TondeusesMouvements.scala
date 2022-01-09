package mowitnow

import java.io.{FileNotFoundException, IOException}
import scala.io.Source.fromResource

object TondeusesMouvements  extends App {
  // Gestion des exceptions
  try {
    /* Importer le fichier des instructions livrées à la tondeuse sous forme d'une liste
    où Chaque ligne du fichier correspondra à un élément d'une liste nommée "instructions"*/

    var instructions = fromResource("./Instructions.txt").getLines.toList //la liste aura la forme suivante:(5 5, 1 2 N, GAGAGAGAA, 3 3 E, AADAADADDA)

    // Dans cette étape, On indique les dimensions du terrain où  les tondeuses vont se déplacer. Ces derniers sont présentés par des coordonnées  (x , y).
    // Il convient donc de déterminer les valeurs maximales et valeurs minimales que peuvent prendre x et y.
    // Les valeurs maximales de x et y sont déterminées à partir du fichier instructions et correspondent à 5 et 5 respectivement.
    // Par défaut, les valeurs minimales sont fixées à 0,0.

    val xmax: Int = instructions(0).split(" ")(0).toInt
    val ymax: Int = instructions(0).split(" ")(1).toInt
    val xmin: Int = 0
    val ymin: Int = 0


    // Dans notre fichier instructions, On élémine les dimensions de la pelouse car on n'aura pas besoin.
    var déplacementListe = instructions.splitAt(1)._2


    // Extraction des  instructions d'exploration de la pelouse fournie à la tondeuse 1 grâce à la fonction take.
    var tondeuse1 = déplacementListe.take(2)

    /* Extraction des  instructions d'exploration de la pelouse fournie à la tondeuse 2
    grâce à la fonction takeRight (navigation de la liste de droite à gauche) */
    var tondeuse2 = déplacementListe.takeRight(2)


    /* On crée une fonction qui permet le déplacement des tondeuses sur la pelouse
    en respectant  les instructions définis pour tondeuse 1 et tondeuse 2.*/

    def Tonter(nom: String, tondeuse: List[String]) = {
      // extraire l'orientation initiale qu'elle soit N,E,O,S.
      val orientationInitiale: String = tondeuse(0).split(" ")(2).toString
      //extraire les coordonnées initiaux x et y sous forme d'une liste
      val coordonnéeInitiale = List(tondeuse(0).split(" ")(0).toInt, tondeuse(0).split(" ")(1).toInt)
      //extraire le déplacement des tondeuses sur la pelouse (séquence des lettres A,G,D)
      val chemin = tondeuse(1).toList.map(_.toString)

      // On indique la taille de la matrice sur laquelle les tondeuses vont faire évoluer leur déplacement.
      var x: Int = coordonnéeInitiale(0)
      var y: Int = coordonnéeInitiale(1)
      var orient: String = orientationInitiale

      /* Le mouvement de la tondeuse est determinee par trois lettres: A, G, D.
      Ainsi, on crée une boucle pour chacune de ces mouvements pour savoir comment l'orientation de la tondeuse va changer.
       */
      for (pas <- chemin) {
        // Si la tondeuse pivote de 90° à gauche alors les orientations changent ainsi:
        if (pas == "G") {
          orient match {
            case "N" => orient = "O"
            case "E" => orient = "N"
            case "S" => orient = "E"
            case "O" => orient = "S"
            case invalid => orient = "error"
          }
        }
        // Si la tondeuse pivote de 90° à droite alors les orientations changent ainsi:
        else if (pas == "D") {
          orient match {
            case "N" => orient = "E"
            case "E" => orient = "S"
            case "S" => orient = "O"
            case "O" => orient = "N"
            case invalid => orient = "error"
          }
        }
        /*Si la tondeuse fait un A( Avancer), alors  elle avance d'une case dans la direction à laquelle elle fait face sans modifier son orientation.
        Ainsi, les coordonnées (soit x ou y ) vont changer.
         */
        else if (pas == "A") {
          // Avancement vers le nord, x demeure constante et y change (on ajoute 1 car on monte dans le plan)
          if (orient == "N" & y < ymax) y = y + 1
          // Avancement vers l'ouest, y demeure constante et x change (on soustrait 1 car on se déplace à gauche dans le plan)
          else if (orient == "O" & x > xmin) x = x - 1
          // Avancement vers l'est, y demeure constante et x change (on ajoute 1 car on se déplace à droite dans le plan)
          else if (orient == "E" & x < xmax) x = x + 1
          // Avancement vers le sud , x demeure constante et y change (on soustrait 1 car on descend dans le plan)
          else if (orient == "S" & y > ymin) y = y - 1
          /*Au cas où la position après mouvement est en dehors de la pelouse ( les coordonnées dépassent la taille prédéfinie de la dimension de la pelouse)
          la tondeuse ne bouge pas,conserve son orientation et traite la commande suivante.
           */
          else println("tondeuse ne bouge pas")
        }
      }
    println(s"${nom}: ${x}  ${y}  ${orient}")
  }
  // Application de la fonction  Tonter à la tondeuse 1 et à la tondeuse 2 .
  Tonter("Tondeuse 1", tondeuse1)
  Tonter("Tondeuse 2", tondeuse2)

  // Gestion des exceptions
}

catch {
  case e: IOException => println (" IOException a lieu en essayant de lire ce fichier")
  case e: FileNotFoundException => println ("Le fichier spécifié est introuvable :( ")
}


}
