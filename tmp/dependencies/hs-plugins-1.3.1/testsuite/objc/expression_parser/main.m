//
//  main.m
//  PluginExpressionParser
//
//  Created by André Pang on Mon Jun 07 2004.
//  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>

#include "HsFFI.h"

extern void __stginit_PluginEvalAux (void);

int main(int argc, char *argv[])
{
    hs_init(&argc, &argv);
    hs_add_root(__stginit_PluginEvalAux);
    const char *c_argv = (const char *) argv;
    int retval = NSApplicationMain(argc, &c_argv);
    hs_exit();
    return retval;
}

/* vi:sw=4 */

