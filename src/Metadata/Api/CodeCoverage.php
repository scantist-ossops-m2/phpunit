<?php declare(strict_types=1);
/*
 * This file is part of PHPUnit.
 *
 * (c) Sebastian Bergmann <sebastian@phpunit.de>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */
namespace PHPUnit\Metadata\Api;

use function assert;
use function interface_exists;
use function sprintf;
use PHPUnit\Framework\CodeCoverageException;
use PHPUnit\Framework\InvalidCoversTargetException;
use PHPUnit\Metadata\CoversClass;
use PHPUnit\Metadata\CoversFunction;
use PHPUnit\Metadata\CoversMethod;
use PHPUnit\Metadata\Parser\Registry;
use PHPUnit\Metadata\UsesClass;
use PHPUnit\Metadata\UsesFunction;
use PHPUnit\Metadata\UsesMethod;
use SebastianBergmann\CodeUnit\CodeUnitCollection;
use SebastianBergmann\CodeUnit\Exception as CodeUnitException;
use SebastianBergmann\CodeUnit\Mapper;

/**
 * @internal This class is not covered by the backward compatibility promise for PHPUnit
 */
final readonly class CodeCoverage
{
    /**
     * @psalm-param class-string $className
     * @psalm-param non-empty-string $methodName
     *
     * @psalm-return array<string,list<int>>|false
     *
     * @throws CodeCoverageException
     */
    public function linesToBeCovered(string $className, string $methodName): array|false
    {
        if (!$this->shouldCodeCoverageBeCollectedFor($className, $methodName)) {
            return false;
        }

        $codeUnits = CodeUnitCollection::fromList();
        $mapper    = new Mapper;

        foreach (Registry::parser()->forClassAndMethod($className, $methodName) as $metadata) {
            if (!$metadata->isCoversClass() && !$metadata->isCoversMethod() && !$metadata->isCoversFunction()) {
                continue;
            }

            assert($metadata instanceof CoversClass || $metadata instanceof CoversMethod || $metadata instanceof CoversFunction);

            if ($metadata->isCoversClass() || $metadata->isCoversMethod() || $metadata->isCoversFunction()) {
                $codeUnits = $codeUnits->mergeWith($this->mapToCodeUnits($metadata));
            }
        }

        return $mapper->codeUnitsToSourceLines($codeUnits);
    }

    /**
     * @psalm-param class-string $className
     * @psalm-param non-empty-string $methodName
     *
     * @psalm-return array<string,list<int>>
     *
     * @throws CodeCoverageException
     */
    public function linesToBeUsed(string $className, string $methodName): array
    {
        $codeUnits = CodeUnitCollection::fromList();
        $mapper    = new Mapper;

        foreach (Registry::parser()->forClassAndMethod($className, $methodName) as $metadata) {
            if (!$metadata->isUsesClass() && !$metadata->isUsesMethod() && !$metadata->isUsesFunction()) {
                continue;
            }

            assert($metadata instanceof UsesClass || $metadata instanceof UsesMethod || $metadata instanceof UsesFunction);

            if ($metadata->isUsesClass() || $metadata->isUsesMethod() || $metadata->isUsesFunction()) {
                $codeUnits = $codeUnits->mergeWith($this->mapToCodeUnits($metadata));
            }
        }

        return $mapper->codeUnitsToSourceLines($codeUnits);
    }

    /**
     * @psalm-param class-string $className
     * @psalm-param non-empty-string $methodName
     */
    public function shouldCodeCoverageBeCollectedFor(string $className, string $methodName): bool
    {
        $metadataForClass  = Registry::parser()->forClass($className);
        $metadataForMethod = Registry::parser()->forMethod($className, $methodName);

        if ($metadataForMethod->isCoversNothing()->isNotEmpty()) {
            return false;
        }

        if ($metadataForMethod->isCoversClass()->isNotEmpty() ||
            $metadataForMethod->isCoversFunction()->isNotEmpty()) {
            return true;
        }

        if ($metadataForClass->isCoversNothing()->isNotEmpty()) {
            return false;
        }

        return true;
    }

    /**
     * @throws InvalidCoversTargetException
     */
    private function mapToCodeUnits(CoversClass|CoversFunction|CoversMethod|UsesClass|UsesFunction|UsesMethod $metadata): CodeUnitCollection
    {
        $mapper = new Mapper;

        try {
            return $mapper->stringToCodeUnits($metadata->asStringForCodeUnitMapper());
        } catch (CodeUnitException $e) {
            if ($metadata->isCoversClass() || $metadata->isUsesClass()) {
                if (interface_exists($metadata->className())) {
                    $type = 'Interface';
                } else {
                    $type = 'Class';
                }
            } else {
                $type = 'Function';
            }

            throw new InvalidCoversTargetException(
                sprintf(
                    '%s "%s" is not a valid target for code coverage',
                    $type,
                    $metadata->asStringForCodeUnitMapper(),
                ),
                $e->getCode(),
                $e,
            );
        }
    }
}
