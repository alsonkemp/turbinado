/* MyDocument */

#import <Cocoa/Cocoa.h>

#include "RunHaskell.h"

@interface MyDocument : NSDocument
{
    IBOutlet id evaluation;
    IBOutlet id expressionEntry;
    IBOutlet id parser;
}
- (IBAction)chooseParser:(id)sender;
- (IBAction)evaluateExpression:(id)sender;

@end
