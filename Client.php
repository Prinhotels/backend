<?php 

require_once 'gen-php/Types.php';
require_once 'gen-php/Season.php';
/* Dependencies. In the proper order. */  
require_once 'Thrift/Transport/TTransport.php';
require_once 'Thrift/Transport/TSocket.php';
require_once 'Thrift/Protocol/TProtocol.php';
require_once 'Thrift/Protocol/TBinaryProtocol.php';
require_once 'Thrift/Transport/TBufferedTransport.php';
require_once 'Thrift/Type/TMessageType.php';
require_once 'Thrift/Factory/TStringFuncFactory.php';
require_once 'Thrift/StringFunc/TStringFunc.php';
require_once 'Thrift/StringFunc/Core.php';
require_once 'Thrift/Type/TType.php';
require_once 'Thrift/Exception/TException.php';
require_once 'Thrift/Exception/TTransportException.php';
use Thrift\Protocol\TBinaryProtocol;
use Thrift\Transport\TSocket;
use Thrift\Transport\TSocketPool;
use Thrift\Transport\TFramedTransport;
use Thrift\Transport\TBufferedTransport;

// Several things might go wrong
try {
    // Create a thrift connection (Boiler plate)
    $socket = new TSocket('localhost', '4390');
    $transport = new TBufferedTransport($socket);
    $protocol = new TBinaryProtocol($transport);
 
    // Create a calculator client
    $client = new SeasonClient($protocol);
 
    $transport->open();
    // Open up the connection
    /*
    print_r($rowTPR);
    $rowTPR->write($protocol);
    $transport->flush();
    $rowTPR->read($protocol);
    print_r($rowTPR);
    */
    print_r($client->getInfo(1, "2012-05-01", "2013-04-30"));
 
    // And finally, we close the thrift connection
    $transport->close();
 /*
} catch (ArithmaticException $ae) {
    // performed an illegal operation, like 10/0
    echo "ArithmatixException: ".$ae->msg."\r\n";
 
} catch (MatrixException $mx) {
    // performed an illegal matrix operation
    echo "MatrixException: ".$mx->msg."\r\n";
 */
} catch (TException $tx) {
    // a general thrift exception, like no such server
    echo "ThriftException: ".$tx->getMessage()."\r\n";
}