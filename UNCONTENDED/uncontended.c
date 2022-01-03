// using sccz80 1.99c
//lesson for loading playing Vortex tracker music in banked memory
//
// need to rework and replace zx0 with newer version - done
//see https://github.com/einar-saukas/ZX0
// need to rework bankswitching code.  look at lesson 10 for solution

#include <arch/zx.h>
#include <stdio.h>//standard input output
#include <input.h>//used for detecting key inputs
//#include <intrinsic.h>//temp to place labels
//powerful troubleshooting tool
        //intrinsic_label(border_start);
        //intrinsic_label(border_end);

#include "variables.h"
#include "externs.h"
#include "uncontended.h"


void main ()
{
    SETUP_IM2();// set up for the interrupt mode 2
    // this will point the I register to IM2 Vector

    disable_interupts();

    bank_switch (0);



//    disable_interupts();
//    intrinsic_label(VT_SONG);
    VT_SONG = 0;//attention new
    //choose song 0-2

//    intrinsic_label(VT_INIT_start);
    VT_LOOP = 2;//attention new
    //1 = play once
    //2 = repeat

//    intrinsic_label(VT_Setup_start);
    VT_Setup();//attention new

    enable_interupts();



    zx_cls(PAPER_WHITE);
    printf("\x16\x01\x02");
    printf ("Main () running in uncontended\n");

    printf ("_Variable_in_contended = %d\n", Variable_in_contended);
    //printf ("_Variable_in_Bank0 = %d\n", Variable_in_Bank0);

    printf ("tom = %d\n", tom);
    printf ("dick = %d\n", dick);
    printf ("harry = %d\n", harry);
    printf ("eve = %u\n", eve);
    printf ("bob = %u\n\n", bob);

    harry = add_two_numbers (tom, dick);
    printf ("Add 2 numbers %u + %u = %u\n", tom, dick, harry);

    harry = multiply2numbers (Variable_in_contended, bob);
    printf ("Multiply 2 numbers %u * %u = %u\n", Variable_in_contended, bob, harry);

    printf("\n\nPress 1-3 to change songs\n");

    pauseLoop(5000);//attention new

    printf("Does the music play now?");


    bank_switch (0);//we will need to be in bank 0


    enable_interupts ();

    while(1)
    {
        //pauseLoop(100000);
        showScreen1();
        showScreen2();
        showScreen3();
        showScreen4();
        showScreen5();
        showScreen6();
        showScreen7();
    }
}


// must have blank line at end


