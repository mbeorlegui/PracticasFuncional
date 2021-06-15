# La casa de papel

[Aqui](https://docs.google.com/document/d/1YcSNxHoNAq9woOQqkBI-N8LCA7p0-GCEXHffSHaZuF0/edit#heading=h.ywq13rwfdf0v) se encuentra la consigna del parcial.

Se está por producir el golpe más grande de la historia, atracar la casa de moneda y timbre.
El Profesor nos ha pedido ayuda para llevar a cabo esta ambiciosa operación, en especial, lo que ocurre dentro del edificio.

De antemano, nos piden que modelemos a los **ladrones** y los **rehenes**.
Sabemos que el atraco lo realizan ladrones de profesión, de los cuales se conocen su nombre, habilidades y las armas que lleva.

De los rehenes conocemos su nombre, su nivel de complot, su nivel de miedo, y su plan contra los ladrones, la cual puede involucrar a algún otro rehén.

Las únicas armas que llegaron a conseguir con su presupuesto son pistolas y ametralladoras, pero seguramente pueden conseguir más en el futuro:
- **Pistola**: reduce el nivel de complot de un rehén en 5 veces su calibre, y aumenta su miedo en 3 por la cantidad de letras de su nombre
- **Ametralladora**: siempre reduce el nivel de complot a la mitad, y aumenta su miedo en la cantidad de balas que le quedan.

Nuestros carismáticos ladrones pasaron meses preparándose para ejecutar este plan, durante los cuales acordaron formas de intimidar a los rehenes para desalentarlos de rebelarse:
- **Disparos**: disparar al techo como medida disuasiva. Se usa el arma que le genera más miedo al rehén intimidado.
- **Hacerse el malo**: 
  - Cuando el que se hace el malo es Berlín, aumenta el miedo del rehén tanto como la cantidad de letras que sumen sus habilidades.
  - Cuando Río intenta hacerse el malo, le sale mal y en cambio aumenta el nivel de complot del rehén en 20. 
  - En otros casos, el miedo del rehén sube en 10. 

A los rehenes no les gusta ser rehenes, por eso intentan rebelarse contra los ladrones, siempre que tengan más complot que miedo, ideando planes como:
- **Atacar al ladrón**: le quita tantas armas como la cantidad de letras del nombre de su compañero de ataque, dividido por 10.
- **Esconderse**: Hace que un ladrón pierda una cantidad de armas igual a su cantidad de habilidades dividido 3. 
----

### Se pide (desarrollando y explicitando el tipo):
1. Modelar a los siguientes personajes:
    - **tokio**, sabe hacer el “trabajo psicológico”, y “entrar en moto”. Lleva dos pistolas calibre 9 milímetros y una ametralladora de 30 balas.
    - **profesor**, sabe “disfrazarse de linyera”, “disfrazarse de payaso” y “estar siempre un paso adelante”. No tiene armas
    - **pablo**, el cual tiene 40 de complot y 30 de miedo. Su plan es esconderse.
    - **arturito**, tiene 70 de complot y 50 de miedo. Su plan es esconderse y luego atacar con pablo.
2. Saber si un ladrón **es inteligente**. Ocurre cuando tiene más de dos habilidades, además el Profesor es la mente maestra, por lo que indudablemente es inteligente.
3. Que un ladrón **consiga un arma** nueva, y se la agregue a las que ya tiene.
4. Que un ladrón **intimide a** un rehén, usando alguno de los métodos planeados.
5. Que un ladrón **calme las aguas**, disparando al techo frente a un grupo de rehenes, de los cuales se calman los que tengan más de 60 de complot.
6. Saber si un ladrón **puede escaparse** de la policía. Esto se cumple cuando alguna de las habilidades del ladrón empieza con “disfrazarse de”.
7. Saber si la **cosa pinta mal**, que es cuando dados unos ladrones y unos rehenes, el nivel de complot promedio de los rehenes es mayor al nivel de miedo promedio multiplicado por la cantidad de armas de los ladrones.
8. Que los rehenes **se rebelen** contra un ladrón, usando el plan que tengan en mente. Saben que es mala idea, por lo que todos pierden 10 de complot antes de comenzar la rebelión
9. Ejecutar el **Plan Valencia**, que consiste en escapar con la mayor cantidad de dinero posible. El dinero conseguido, es igual a $1000000, multiplicado por la cantidad de armas que tengan todos los ladrones en total, luego de que:
  - Se armen todos con una ametralladora de 45 balas
  - Todos los rehenes se rebelen contra todos los ladrones
10. ¿Se puede ejecutar el plan valencia si uno de los ladrones tiene una cantidad infinita de armas? Justifique.
11. ¿Se puede ejecutar el plan valencia si uno de los ladrones tiene una cantidad infinita de habilidades? Justifique.
----
### Sugerencias para el examen:
  - Maximizar el uso de funciones compuestas y de orden superior.
  - Tener en cuenta la expresividad, tanto en las funciones como en sus parámetros.
  - Evitemos la repetición de lógica.
