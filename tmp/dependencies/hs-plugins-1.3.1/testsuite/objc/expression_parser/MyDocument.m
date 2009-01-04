#import "MyDocument.h"

@implementation MyDocument

- (NSString *)windowNibName {
    return @"MyDocument";
}

- (NSData *)dataRepresentationOfType:(NSString *)type {
    return nil;
}

- (BOOL)loadDataRepresentation:(NSData *)data ofType:(NSString *)type {
    return NO;
}


- (IBAction)chooseParser:(id)sender
{
    int result;
    NSArray *fileTypes = [NSArray arrayWithObject:@"hs"];
    NSOpenPanel *oPanel = [NSOpenPanel openPanel];
    
    result = [oPanel runModalForDirectory:nil file:nil types:fileTypes];
    if (result == NSOKButton)
    {
        NSArray *filesToOpen = [oPanel filenames];
        [parser setStringValue:[filesToOpen objectAtIndex:0]];
    }
}

- (IBAction)evaluateExpression:(id)sender
{
    NSLog(@"evaluateExpression");
    NSString *filePathNSS = [parser stringValue];
    char *filePath = [filePathNSS cString];
    
    NSString *expressionNSS = [[expressionEntry textStorage] string];
    char *expression = [expressionNSS cString];
    
    NSLog (@"filePath:%s expression:%s", filePath, expression);
    
    char *result = evalhaskell_CString(filePath, expression);
    NSString *resultNSS = [NSString stringWithCString:result];
    NSAttributedString *resultNSAS = [[NSAttributedString alloc]
	                              initWithString:resultNSS
				          attributes:nil];
    [[evaluation textStorage] setAttributedString:resultNSAS];

}

@end
